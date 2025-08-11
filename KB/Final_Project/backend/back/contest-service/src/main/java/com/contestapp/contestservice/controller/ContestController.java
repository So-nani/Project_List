package com.contestapp.contestservice.controller;

import java.util.ArrayList;
import java.util.UUID;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.contestapp.contestservice.dto.request.ContestCreateRequest;
import com.contestapp.contestservice.dto.request.ContestUpdateRequest;
import com.contestapp.contestservice.dto.response.ContestResponse;
import com.contestapp.contestservice.entity.Contest;
import com.contestapp.contestservice.entity.ContestStatus;
import com.contestapp.contestservice.service.ContestService;

import lombok.RequiredArgsConstructor;
import lombok.val;
import lombok.extern.slf4j.Slf4j;

/**
 * Contest 관련 REST API 컨트롤러
 * 
 * [역할]
 * - 대회 관련 HTTP 요청을 받아 적절한 비즈니스 로직으로 전달
 * - 요청 파라미터 검증 및 응답 데이터 변환
 * - RESTful API 설계 원칙에 따른 엔드포인트 제공
 * 
 * [API 설계 원칙]
 * - REST 규약 준수: GET(조회), POST(생성), PUT(수정), DELETE(삭제)
 * - 명확한 URL 구조: /api/contests/{action}
 * - 표준 HTTP 상태 코드 사용: 200(성공), 404(없음), 400(잘못된 요청)
 * 
 * [응답 형식]
 * - 성공: ResponseEntity<T> with 200 OK
 * - 단일 객체: ContestResponse
 * - 목록: Page<ContestResponse> (페이징 정보 포함)
 * 
 * [에러 처리]
 * - IllegalArgumentException → 404 Not Found (향후 @ExceptionHandler 추가 예정)
 * - 잘못된 파라미터 → 400 Bad Request (Spring Validation 추가 예정)
 * 
 * [팀원 확장 가이드]
 * 1. 새 API 추가시 RESTful 규칙 준수
 * 2. 요청/응답 DTO 사용으로 엔티티 직접 노출 금지
 * 3. @Valid 어노테이션으로 요청 데이터 검증
 * 4. @ExceptionHandler로 일관된 에러 응답 제공
 */
@RestController // @Controller + @ResponseBody: JSON 응답을 자동으로 생성
@RequestMapping("/api/contests") // 기본 URL 경로: /api/contests
@RequiredArgsConstructor // Lombok: final 필드들을 파라미터로 받는 생성자 자동 생성
@Slf4j
public class ContestController {

    /**
     * Contest 비즈니스 로직 처리 서비스
     * 
     * [의존성 주입] @RequiredArgsConstructor에 의해 생성자 주입
     * [사용 목적] 실제 비즈니스 로직 실행을 위한 서비스 계층 호출
     */
    private final ContestService contestService;

    /**
     * 대회 목록 조회 API
     * 
     * [HTTP 메서드] GET
     * [URL] /api/contests
     * [기능] 페이징, 정렬, 필터링이 가능한 대회 목록 조회
     * 
     * [쿼리 파라미터]
     * - category: 카테고리 ID로 필터링 (선택사항)
     * - keyword: 제목, 설명, 주최자로 검색 (선택사항)
     * - page: 페이지 번호, 0부터 시작 (기본값: 0)
     * - size: 페이지 크기 (기본값: 10, 최대 100 권장)
     * - sortBy: 정렬 기준 필드 (기본값: createdAt)
     * - sortDir: 정렬 방향 (기본값: desc, asc/desc)
     * 
     * [요청 예시]
     * GET /api/contests → 전체 목록 (최신순)
     * GET /api/contests?category=1 → 카테고리 1번 대회 목록
     * GET /api/contests?keyword=프로그래밍 → 키워드 검색
     * GET /api/contests?page=1&size=20 → 2페이지, 20개씩
     * GET /api/contests?sortBy=startDate&sortDir=asc → 시작일 오름차순
     * 
     * [응답 형식]
     * {
     * "content": [ContestResponse...], // 대회 목록
     * "pageable": {...}, // 페이징 정보
     * "totalElements": 100, // 전체 대회 수
     * "totalPages": 10, // 전체 페이지 수
     * "number": 0, // 현재 페이지 번호
     * "size": 10, // 페이지 크기
     * "first": true, // 첫 페이지 여부
     * "last": false // 마지막 페이지 여부
     * }
     * 
     * [프론트엔드 활용]
     * - 대회 목록 페이지의 메인 데이터
     * - 무한 스크롤 또는 페이지네이션 구현
     * - 검색 및 필터링 기능
     * 
     * [성능 고려사항]
     * - 페이징으로 메모리 사용량 제한
     * - size 파라미터는 100 이하로 제한 권장 (향후 검증 추가)
     * - 캐싱 적용 고려 (Redis 등)
     * 
     * @param category 카테고리 ID (null 가능)
     * @param keyword  검색 키워드 (null 가능)
     * @param page     페이지 번호 (0부터 시작)
     * @param size     페이지 크기
     * @param sortBy   정렬 기준 필드명
     * @param sortDir  정렬 방향 (asc/desc)
     * @return 대회 목록과 페이징 정보가 포함된 응답
     */
    @GetMapping
    public ResponseEntity<Page<ContestResponse>> getContests(
            @RequestParam(required = false) Long category, // 선택적 카테고리 필터
            @RequestParam(required = false) String keyword, // 선택적 키워드 검색
            @RequestParam(defaultValue = "0") int page, // 기본 첫 페이지
            @RequestParam(defaultValue = "10") int size, // 기본 10개씩
            @RequestParam(defaultValue = "createdAt") String sortBy, // 기본 생성일 정렬
            @RequestParam(defaultValue = "desc") String sortDir) { // 기본 내림차순

        // 정렬 방향 결정: "desc"이면 내림차순, 그 외는 오름차순
        Sort.Direction direction = sortDir.equalsIgnoreCase("desc")
                ? Sort.Direction.DESC
                : Sort.Direction.ASC;

        // Pageable 객체 생성: 페이지 정보 + 정렬 조건
        Pageable pageable = PageRequest.of(page, size, Sort.by(direction, sortBy));

        // 서비스 계층에서 비즈니스 로직 실행
        Page<Contest> contests = contestService.findContests(category, keyword, pageable);

        // Entity를 DTO로 변환: Page.map()으로 각 요소 변환
        Page<ContestResponse> contestResponses = contests.map(ContestResponse::from);

        // 200 OK와 함께 변환된 응답 반환
        return ResponseEntity.ok(contestResponses);
    }

    /**
     * 대회 상세 조회 API
     * 
     * [HTTP 메서드] GET
     * [URL] /api/contests/{contestId}
     * [기능] 특정 대회의 상세 정보 조회 (카테고리 정보 포함)
     * 
     * [URL 경로 변수]
     * - contestId: 조회할 대회의 UUID
     * 
     * [요청 예시]
     * GET /api/contests/550e8400-e29b-41d4-a716-446655440000
     * 
     * [응답 형식]
     * {
     * "id": "550e8400-e29b-41d4-a716-446655440000",
     * "title": "2024 프로그래밍 챌린지",
     * "description": "Java 프로그래밍 실력을 겨루는...",
     * "organizer": "삼성전자",
     * "startDate": "2024-06-01T10:00:00",
     * "endDate": "2024-06-03T18:00:00",
     * "categories": [
     * {"id": 1, "name": "프로그래밍", "description": "..."}
     * ],
     * "createdAt": "2024-05-01T09:00:00",
     * "updatedAt": "2024-05-15T14:30:00"
     * }
     * 
     * [에러 응답]
     * - 404 Not Found: 대회가 존재하지 않거나 비활성화된 경우
     * - 400 Bad Request: 잘못된 UUID 형식 (Spring이 자동 처리)
     * 
     * [프론트엔드 활용]
     * - 대회 상세 페이지의 메인 데이터
     * - 참가 신청, 즐겨찾기 기능의 기본 정보
     * - 공유하기 기능의 메타데이터
     * 
     * [보안 고려사항]
     * - 비활성화된 대회는 조회 불가 (관리자 전용 API 별도 필요)
     * - UUID 사용으로 대회 ID 추측 공격 방지
     * 
     * [캐싱 고려]
     * - 자주 조회되는 대회는 Redis 캐싱 적용 가능
     * - ETag 헤더로 클라이언트 캐싱 최적화 고려
     * 
     * @param contestId 조회할 대회의 UUID
     * @return 대회 상세 정보 (카테고리 포함)
     * @throws IllegalArgumentException 대회를 찾을 수 없는 경우 (향후 404 응답으로 변환)
     */
    @GetMapping("/{contestId}")
    public ResponseEntity<ContestResponse> getContest(@PathVariable UUID contestId) {
        // 서비스에서 대회 조회 (카테고리 정보 포함)
        Contest contest = contestService.findById(contestId);

        // Entity를 DTO로 변환
        ContestResponse response = ContestResponse.from(contest);

        // 200 OK와 함께 응답 반환
        return ResponseEntity.ok(response);
    }

    /**
     * 서비스 상태 확인 API (테스트/모니터링용)
     * 
     * [HTTP 메서드] GET
     * [URL] /api/contests/test
     * [기능] Contest Service의 실행 상태 확인
     * 
     * [용도]
     * - 헬스체크: 서비스가 정상 실행 중인지 확인
     * - API Gateway의 라우팅 테스트
     * - 개발/테스트 환경에서 연결 확인
     * 
     * [요청 예시]
     * GET /api/contests/test
     * 
     * [응답 예시]
     * "Contest Service is running!"
     * 
     * [활용 상황]
     * - 새로운 배포 후 서비스 정상 동작 확인
     * - 로드밸런서의 헬스체크 엔드포인트
     * - 프론트엔드 개발시 API 연결 테스트
     * 
     * [향후 개선]
     * - 데이터베이스 연결 상태 포함
     * - 서비스 버전 정보 추가
     * - 의존성 서비스 상태 확인
     * 
     * @return 서비스 상태 메시지
     */
    @GetMapping("/test")
    public ResponseEntity<String> test() {
        return ResponseEntity.ok("Contest Service is running!");
    }

    /*
     * [향후 확장 예정 API들]
     * 
     * 1. 대회 생성 API (관리자용)
     * 
     * @PostMapping
     * public ResponseEntity<ContestResponse> createContest(@Valid @RequestBody
     * ContestCreateRequest request) {
     * // 요청 데이터 검증, 대회 생성, 응답 반환
     * }
     * 
     * 2. 대회 수정 API (관리자용)
     * 
     * @PutMapping("/{contestId}")
     * public ResponseEntity<ContestResponse> updateContest(@PathVariable UUID
     * contestId,
     * 
     * @Valid @RequestBody ContestUpdateRequest request) {
     * // 대회 존재 확인, 권한 검증, 수정 후 응답
     * }
     * 
     * 3. 대회 삭제 API (관리자용)
     * 
     * @DeleteMapping("/{contestId}")
     * public ResponseEntity<Void> deleteContest(@PathVariable UUID contestId) {
     * // 소프트 삭제 (isActive = false)
     * }
     * 
     * 4. 대회 즐겨찾기 API
     * 
     * @PostMapping("/{contestId}/favorite")
     * public ResponseEntity<Void> addFavorite(@PathVariable UUID contestId,
     * 
     * @RequestHeader("X-User-ID") UUID userId) {
     * // 즐겨찾기 추가
     * }
     * 
     * 5. 대회 참가 신청 API
     * 
     * @PostMapping("/{contestId}/participate")
     * public ResponseEntity<ParticipationResponse> participate(@PathVariable UUID
     * contestId,
     * 
     * @RequestHeader("X-User-ID") UUID userId,
     * 
     * @Valid @RequestBody ParticipationRequest request) {
     * // 참가 신청 처리
     * }
     * 
     * 6. 인기 대회 목록 API
     * 
     * @GetMapping("/popular")
     * public ResponseEntity<Page<ContestResponse>> getPopularContests(Pageable
     * pageable) {
     * // 참가자 수, 즐겨찾기 수 기준 정렬
     * }
     * 
     * 7. 카테고리별 대회 통계 API
     * 
     * @GetMapping("/stats/categories")
     * public ResponseEntity<List<CategoryStatsResponse>> getCategoryStats() {
     * // 카테고리별 대회 수, 참가자 수 통계
     * }
     */

    // 1. 대회 생성 API (관리자용)

    @PostMapping
    public ResponseEntity<ContestResponse> createContest(
            @val @RequestBody ContestCreateRequest request,
            @RequestHeader("X-User-ID") UUID userId) { // X-User-ID 헤더에서 사용자 ID를 받습니다.
        // 요청 데이터 검증, 대회 생성, 응답 반환
        if (request.getCategoryIds() == null) {
            request.setCategoryIds(new ArrayList<>());
        }
        Contest contest = contestService.createContest(request, userId); // userId를 서비스로 전달
        ContestResponse response = ContestResponse.from(contest);
        return ResponseEntity.status(201).body(response);
    }

    // // 2. 대회 수정 API (관리자용)
    @PutMapping("/{contestId}")
    public ResponseEntity<ContestResponse> updateContest(@PathVariable UUID contestId,
            @RequestBody ContestUpdateRequest request) {
        // 대회 존재 확인 및 수정
        Contest updatedContest = contestService.updateContest(contestId, request);
        ContestResponse response = ContestResponse.from(updatedContest);
        return ResponseEntity.ok(response);
    }

    // 3. 대회 삭제 API (관리자용)
    @DeleteMapping("/{contestId}/deactivate")
    public ResponseEntity<Void> deleteContest(@PathVariable UUID contestId) {
        System.out.println("DELETE 요청 처리: 대회 ID - " + contestId);
        try {
            contestService.deactivateContest(contestId);
            return ResponseEntity.noContent().build();
        } catch (IllegalArgumentException e) {
            // ID를 찾을 수 없는 경우
            log.warn("DELETE 요청 실패: 대회 ID - {}, 메시지: {}", contestId, e.getMessage()); // log.warn 사용
            return ResponseEntity.notFound().build(); // 404 Not Found
        } catch (Exception e) { // 모든 다른 예외를 잡아서 500으로 처리 (일반적인 RuntimeException 포함)
            log.error("대회 비활성화 중 예상치 못한 오류 발생: 대회 ID - {}", contestId, e); // log.error 사용
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build(); // 500 Internal Server Error
        }
    }



  
  //------------------------수경------------------------
    @GetMapping("/status")
    public Page<ContestResponse> getContestsByStatus(
            @RequestParam(required = false) ContestStatus status,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "endDate") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir
    ) {
        return contestService.getContests(status, page, size, sortBy, sortDir);
    }

    // GET /api/categories/it/contests?status=CLOSING_SOON&page=0&size=10&sortBy=title

    //요청 URL	의미
    // /api/contests/status?status=CLOSING_SOON	전체 공모전 중 마감임박인 것
    // /api/categories/1/contests?status=CLOSING_SOON	카테고리 1에 속한 공모전 중 마감임박인 것
    // /api/categories/1/contests	카테고리 1에 속한 모든 공모전

    @GetMapping("/region")
    public ResponseEntity<Page<ContestResponse>> getContestsByRegion(
            @RequestParam(required = false) String regionSi,
            @RequestParam(required = false) String regionGu,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {

        Page<Contest> contestPage = contestService.findContestsByRegion(regionSi, regionGu, pageable);
        Page<ContestResponse> responsePage = contestPage.map(ContestResponse::from);

        return ResponseEntity.ok(responsePage);
    }


    //------------------------수경------------------------
}

