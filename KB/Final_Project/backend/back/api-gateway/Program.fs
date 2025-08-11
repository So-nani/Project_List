open System
open System.Net.Http
open System.Linq
open System.Threading.Tasks
open System.Text.Json
open System.IdentityModel.Tokens.Jwt
open System.Security.Claims
open System.Text
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.IdentityModel.Tokens
open Giraffe
open StackExchange.Redis

/// Contains functions for handling reverse proxy logic.
module Proxy =
    let forwardRequest (downstreamBaseUri: string) : HttpHandler =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let httpClientFactory = ctx.RequestServices.GetService<IHttpClientFactory>()
                let client = httpClientFactory.CreateClient()
                let downstreamUri = Uri(downstreamBaseUri + ctx.Request.Path.Value + ctx.Request.QueryString.Value)
                use downstreamRequest = new HttpRequestMessage(new HttpMethod(ctx.Request.Method), downstreamUri)

                if ctx.Request.ContentLength.HasValue && ctx.Request.ContentLength.Value > 0L then
                    downstreamRequest.Content <- new StreamContent(ctx.Request.Body)

                ctx.Request.Headers
                |> Seq.iter (fun header -> 
                    if not (downstreamRequest.Headers.TryAddWithoutValidation(header.Key, header.Value.ToArray())) then
                        if downstreamRequest.Content <> null then
                            downstreamRequest.Content.Headers.TryAddWithoutValidation(header.Key, header.Value.ToArray()) |> ignore)

                use! downstreamResponse = client.SendAsync(downstreamRequest, HttpCompletionOption.ResponseHeadersRead, ctx.RequestAborted)

                // --- DEBUG: Log downstream headers ---
                printfn "--- Downstream Response Headers ---"
                downstreamResponse.Headers
                |> Seq.append downstreamResponse.Content.Headers
                |> Seq.iter (fun header -> 
                    let valueStr = String.concat ", " header.Value
                    printfn $"Key: {header.Key}, Value: {valueStr}"
                )
                printfn "-----------------------------------"
                // --- END DEBUG ---

                ctx.Response.StatusCode <- int downstreamResponse.StatusCode

                downstreamResponse.Headers
                |> Seq.append downstreamResponse.Content.Headers
                |> Seq.filter (fun header -> not (header.Key.Equals("Transfer-Encoding", StringComparison.OrdinalIgnoreCase)))
                |> Seq.iter (fun header -> ctx.Response.Headers.Add(header.Key, header.Value.ToArray()))

                let! responseStream = downstreamResponse.Content.ReadAsStreamAsync()
                do! responseStream.CopyToAsync(ctx.Response.Body, ctx.RequestAborted)
                return Some ctx
            }

/// Contains functions for handling JWT authentication.
module Auth =
    
    /// JWT 토큰에서 추출할 사용자 정보
    type UserInfo = {
        UserId: string
        Username: string
        Role: string option
    }
    
    /// 인증 결과
    type AuthResult = 
        | Success of UserInfo
        | TokenNotFound
        | InvalidToken
        | TokenExpired
        | RedisError of string
    
    /// 쿠키에서 JWT 토큰 추출 (순수 함수)
    let extractTokenFromCookie (cookies: IRequestCookieCollection) : string option =
        match cookies.TryGetValue "auth_token" with
        | true, token when not (String.IsNullOrEmpty token) -> Some token
        | _ -> None
    
    /// JWT 시크릿 키 설정
    let getJwtSecret () = "contestapp-super-secret-jwt-key-2024-auth-server"
    
    /// JWT 토큰 검증 (서명 및 만료시간 확인)
    let validateJwtToken (token: string) : Result<UserInfo, string> =
        try
            let handler = JwtSecurityTokenHandler()
            let jwtSecret = getJwtSecret()
            let key = SymmetricSecurityKey(Encoding.UTF8.GetBytes(jwtSecret))
            
            let validationParameters = TokenValidationParameters(
                ValidateIssuerSigningKey = true,
                IssuerSigningKey = key,
                ValidateIssuer = false,
                ValidateAudience = false,
                ValidateLifetime = true,  // 만료시간 검증 활성화
                ClockSkew = TimeSpan.Zero  // 시간 오차 허용 안함
            )
            
            let mutable validatedToken: SecurityToken = null
            let principal = handler.ValidateToken(token, validationParameters, &validatedToken)
            
            let getUserClaim claimType =
                principal.Claims 
                |> Seq.tryFind (fun c -> c.Type = claimType)
                |> Option.map (fun c -> c.Value)
            
            let userId = getUserClaim "user_id" |> Option.defaultValue ""
            let username = getUserClaim "username" |> Option.defaultValue ""
            let role = getUserClaim "role"
            
            if String.IsNullOrEmpty userId then
                Error "Invalid token: missing user ID"
            else
                Ok { UserId = userId; Username = username; Role = role }
        with
        | :? SecurityTokenExpiredException -> Error "Token expired"
        | :? SecurityTokenInvalidSignatureException -> Error "Invalid token signature"
        | ex -> Error $"JWT validation error: {ex.Message}"
    
    /// Redis에서 JWT 토큰 검증
    let verifyTokenInRedis (db: IDatabase) (token: string) : Task<bool> =
        task {
            try
                let tokenKey = $"token:{token}"
                let! exists = db.KeyExistsAsync tokenKey
                return exists
            with
            | ex -> 
                printfn $"Redis error: {ex.Message}"
                return false
        }
    
    /// Auth Server에서 토큰 갱신 요청
    let refreshTokens (httpClientFactory: IHttpClientFactory) (ctx: HttpContext) : Task<bool> =
        task {
            try
                let client = httpClientFactory.CreateClient()
                let authServerUrl = "http://contestapp-auth-server:60000/auth/refresh"
                
                // 현재 요청의 쿠키들을 그대로 전달
                use request = new HttpRequestMessage(HttpMethod.Post, authServerUrl)
                
                // 쿠키 헤더 복사
                if ctx.Request.Headers.ContainsKey("Cookie") then
                    request.Headers.Add("Cookie", ctx.Request.Headers.["Cookie"].ToArray())
                
                printfn $"Attempting token refresh for request: {ctx.Request.Path}"
                
                use! response = client.SendAsync(request)
                
                if response.IsSuccessStatusCode then
                    // Set-Cookie 헤더들을 클라이언트 응답에 복사
                    if response.Headers.Contains("Set-Cookie") then
                        let setCookies = response.Headers.GetValues("Set-Cookie")
                        for cookie in setCookies do
                            ctx.Response.Headers.Add("Set-Cookie", cookie)
                    
                    printfn "Token refresh successful"
                    return true
                else
                    printfn $"Token refresh failed with status: {response.StatusCode}"
                    return false
            with
            | ex ->
                printfn $"Token refresh error: {ex.Message}"
                return false
        }
    
    /// 사용자 정보를 헤더로 추가 (순수 함수)
    let addUserHeaders (userInfo: UserInfo) (headers: IHeaderDictionary) : unit =
        headers.Add("X-User-ID", userInfo.UserId)
        headers.Add("X-User-Username", userInfo.Username)
        userInfo.Role |> Option.iter (fun role -> headers.Add("X-User-Role", role))
    
    /// JWT 인증 파이프라인 (Railway-Oriented Programming)
    let authenticateRequest (db: IDatabase) (ctx: HttpContext) : Task<AuthResult> =
        task {
            // 1. 쿠키에서 토큰 추출
            match extractTokenFromCookie ctx.Request.Cookies with
            | None -> return TokenNotFound
            | Some token ->
                // 2. JWT 서명 및 만료시간 검증 (먼저 수행)
                match validateJwtToken token with
                | Error "Token expired" -> return TokenExpired
                | Error "Invalid token signature" -> return InvalidToken
                | Error _ -> return InvalidToken
                | Ok userInfo ->
                    // 3. Redis에서 토큰 검증 (추가 보안 검증)
                    let! isValidInRedis = verifyTokenInRedis db token
                    if not isValidInRedis then
                        return InvalidToken
                    else
                        return Success userInfo
        }
    
    /// 인증 미들웨어 (자동 토큰 갱신 포함)
    let requireAuth (next: HttpFunc) (ctx: HttpContext) : Task<HttpContext option> =
        task {
            let redis = ctx.RequestServices.GetService<IConnectionMultiplexer>()
            let db = redis.GetDatabase()
            
            let! authResult = authenticateRequest db ctx
            
            match authResult with
            | Success userInfo ->
                // 사용자 정보를 헤더로 추가
                addUserHeaders userInfo ctx.Request.Headers
                return! next ctx
                
            | TokenExpired ->
                // 토큰 만료시 자동 갱신 시도
                printfn "Access token expired, attempting refresh..."
                let httpClientFactory = ctx.RequestServices.GetService<IHttpClientFactory>()
                let! refreshSuccessful = refreshTokens httpClientFactory ctx
                
                if refreshSuccessful then
                    // 갱신 성공 - 새 토큰으로 다시 인증 시도
                    let! newAuthResult = authenticateRequest db ctx
                    match newAuthResult with
                    | Success newUserInfo ->
                        addUserHeaders newUserInfo ctx.Request.Headers
                        printfn $"Token refresh and re-authentication successful for user: {newUserInfo.Username}"
                        return! next ctx
                    | _ ->
                        printfn "Re-authentication failed after token refresh"
                        ctx.Response.StatusCode <- StatusCodes.Status401Unauthorized
                        do! ctx.Response.WriteAsync "Unauthorized: Re-authentication failed after token refresh."
                        return None
                else
                    // 갱신 실패 - 401 반환
                    printfn "Token refresh failed"
                    ctx.Response.StatusCode <- StatusCodes.Status401Unauthorized
                    do! ctx.Response.WriteAsync "Unauthorized: Token expired and refresh failed."
                    return None
                
            | TokenNotFound ->
                ctx.Response.StatusCode <- StatusCodes.Status401Unauthorized
                do! ctx.Response.WriteAsync "Unauthorized: JWT token not found."
                return None
                
            | InvalidToken ->
                ctx.Response.StatusCode <- StatusCodes.Status401Unauthorized  
                do! ctx.Response.WriteAsync "Unauthorized: Invalid JWT token."
                return None
                
            | RedisError msg ->
                ctx.Response.StatusCode <- StatusCodes.Status500InternalServerError
                do! ctx.Response.WriteAsync $"Internal server error: {msg}"
                return None
        }

// The main web application handler, composed of multiple routes.
let webApp (serviceProvider: IServiceProvider) : HttpHandler =
    let userServiceUrl = "http://contestapp-user-service:8081"
    let contestServiceUrl = "http://contestapp-contest-service:8083" //컨테스트 서비스 주소 추가
    let teamServiceUrl = "http://contestapp-team-service:8086" //팀 서비스 주소 추가
    let aiServiceUrl = "http://contestapp-ai-service:8087" //AI 서비스 주소 추가

    choose [
        // === Public Routes ===
        GET >=> route "/api/health" >=> text "API Gateway is healthy."
        GET >=> routeCi "/api/users/asdf" >=> Proxy.forwardRequest userServiceUrl
        GET >=> routeCi "/api/contests/test" >=> Proxy.forwardRequest contestServiceUrl //인증이 필요없는 api는 따로 여기에
        routeStartsWithCi "/api/teams" >=> Proxy.forwardRequest teamServiceUrl
        
        

        // === Protected Routes ===
        // Any other route that falls through is considered protected.
        Auth.requireAuth >=> choose [
            //user-service 매핑 추가
            routeStartsWithCi "/api/users" >=> Proxy.forwardRequest userServiceUrl
            routeStartsWithCi "/skills" >=> Proxy.forwardRequest userServiceUrl

             //contest-service 매핑 추가
            routeStartsWithCi "/api/contests" >=> Proxy.forwardRequest contestServiceUrl //프록시 루트 추가
            routeStartsWithCi "/api/categories" >=> Proxy.forwardRequest contestServiceUrl
            routeStartsWithCi "/api/mypage" >=> Proxy.forwardRequest contestServiceUrl

            //ai-service 매핑 추가
            routeStartsWithCi "/api/ai" >=> Proxy.forwardRequest aiServiceUrl 

            // team-service 매핑 추가
            routeStartsWithCi "/api/invitations" >=> Proxy.forwardRequest teamServiceUrl           
        ]
        
        // === Fallback ===
        setStatusCode 404 >=> text "Not Found. The requested URL was not routed."
    ]

// Configure services for the DI container.
let configureServices (services: IServiceCollection) =
    // List of allowed origins for CORS. Add your frontend development/production URLs here.
    let allowedOrigins = [| 
        "http://localhost:3000"  // API Gateway static files
        "http://localhost:6001"
    |]

    services.AddCors(fun options ->
        options.AddDefaultPolicy(fun builder ->
            // Allow origins from the list OR allow 'null' for local file testing
            builder.SetIsOriginAllowed(fun origin -> allowedOrigins.Contains(origin) || origin = "null")
                   .AllowAnyMethod()
                   .AllowAnyHeader()
                   .AllowCredentials()
            |> ignore
        )
    ) |> ignore

    let redisConnectionString = "contestapp-redis:6379"
    let redis = ConnectionMultiplexer.Connect(redisConnectionString)
    services.AddSingleton<IConnectionMultiplexer>(redis) |> ignore

    services.AddHttpClient() |> ignore
    services.AddGiraffe() |> ignore

// Configure the application's request pipeline.
let configureApp (app: IApplicationBuilder) =
    let sp = app.ApplicationServices
    app.UseCors() |> ignore

    // Serve test.html as the default file from wwwroot
    app.UseDefaultFiles() |> ignore
    app.UseStaticFiles() |> ignore

    app.UseGiraffe(webApp sp)

[<EntryPoint>]
let main argv =
    Host.CreateDefaultBuilder(argv)
        .ConfigureWebHostDefaults(fun webBuilder ->
            webBuilder
                .ConfigureServices(configureServices)
                .Configure(configureApp)
                |> ignore)
        .Build()
        .Run()
    0 