package com.contestapp.contestservice.dto.response;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import com.contestapp.contestservice.entity.Contest;
import com.contestapp.contestservice.entity.ContestStatus;
import com.fasterxml.jackson.core.JsonProcessingException; // 추가: JSONB 필드 파싱을 위해 필요
import com.fasterxml.jackson.core.type.TypeReference;     // 추가: JSONB 필드 파싱을 위해 필요
import com.fasterxml.jackson.databind.ObjectMapper;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * 대회 정보 응답 DTO (Data Transfer Object)
 * 
 * [역할]
 * - Contest 엔티티의 모든 정보를 클라이언트에게 전달하기 위한 객체
 * - 연관된 카테고리 정보도 함께 포함하여 완전한 대회 정보 제공
 * 
 * [사용 목적]
 * 1. 대회 목록/상세 API의 표준 응답 형식
 * 2. 엔티티 내부 구조 숨김 및 필요한 정보만 노출
 * 3. JSON 직렬화를 통한 RESTful API 응답
 * 
 * [변환 과정]
 * Contest Entity + Categories → ContestResponse DTO → JSON
 * 
 * [API 활용 상황]
 * - GET /api/contests (대회 목록 - 페이징 포함)
 * - GET /api/contests/{id} (대회 상세 조회)
 * - 향후 즐겨찾기 여부, 참가 상태 등 추가 정보 포함 예정
 * 
 * [팀원 확장 가이드]
 * 1. 새 필드 추가시 from() 메서드와 생성자 모두 수정
 * 2. 조건부 표시 필드는 별도 from() 오버로딩 메서드 생성
 * 3. 민감한 정보는 별도 AdminContestResponse 등으로 분리
 */
@Setter
@Getter  // Lombok: 모든 필드의 getter 메서드 자동 생성 (JSON 직렬화용)
@NoArgsConstructor  // Lombok: 기본 생성자 생성 (JSON 역직렬화용)
@AllArgsConstructor  // Lombok: 모든 필드를 파라미터로 받는 생성자 생성
public class ContestResponse {
    
    /**
     * 대회 고유 식별자
     * 
     * [타입] UUID - 전역적으로 유니크한 식별자
     * [용도] 
     * - 대회 상세 조회: GET /api/contests/{id}
     * - 즐겨찾기, 참가 신청시 대회 식별자
     * - 프론트엔드의 라우팅 파라미터
     */
    private UUID id;
    
    /**
     * 대회 제목
     * 
     * [용도] 대회 목록 및 상세 페이지의 주요 제목
     * [활용] 검색 결과 하이라이팅, 소셜 미디어 공유시 제목
     */
    private String title;
    
    /**
     * 대회 상세 설명
     * 
     * [용도] 대회 상세 페이지의 본문 내용
     * [형식] 마크다운, HTML 등 리치 텍스트 지원 가능
     * [null 처리] 설명이 없는 경우 null 반환
     */
    private String description;
    
    /**
     * 대회 주최자/기관명
     * 
     * [용도] 대회 신뢰성 확인 및 주최자 정보 표시
     * [활용] 주최자별 대회 필터링 (향후 구현 예정)
     */
    private String organizer;
    
    /**
     * 대회 시작 일시
     * 
     * [형식] ISO 8601 형식으로 JSON 직렬화 (yyyy-MM-ddTHH:mm:ss)
     * [용도] 대회 상태 판단 (예정/진행중/종료)
     * [활용] 목록 정렬, 필터링 기준
     */
    private LocalDateTime startDate;
    
    /**
     * 대회 종료 일시
     * 
     * [용도] 대회 기간 계산 및 상태 판단
     * [활용] 
     * - D-Day 계산
     * - 종료된 대회 아카이브 처리
     */
    private LocalDateTime endDate;
    
    /**
     * 참가 신청 마감 일시
     * 
     * [nullable] 신청이 필요없는 대회의 경우 null
     * [용도] 신청 가능 여부 판단
     * [활용] "신청 마감" 라벨 표시, 신청 버튼 비활성화
     */
    private LocalDateTime registrationDeadline;
    
    /**
     * 대회 시상 내역
     * 
     * [형식] 자유 텍스트 (향후 구조화된 JSON으로 개선 예정)
     * [예시] "1등 500만원, 2등 300만원, 3등 100만원 + 기념품"
     * [활용] 대회 상세 페이지의 시상 정보 섹션
     */
    private String prizeDescription;
    
    /**
     * 대회 참가 자격 및 요구사항
     * 
     * [용도] 참가자가 사전에 확인해야 할 조건들
     * [예시] "대학생 이상, Java 프로그래밍 경험 필수, 팀 구성 가능"
     * [활용] 대회 상세 페이지의 참가 조건 섹션
     */
    private String requirements;
    
    /**
     * 대회 공식 웹사이트 URL
     * 
     * [용도] 외부 사이트로 이동하는 링크
     * [활용] "자세히 보기", "신청하기" 버튼의 연결 주소
     * [검증] URL 형식 유효성은 컨트롤러에서 처리
     */
    private String websiteUrl;
    
    /**
     * 대회 대표 이미지 URL
     * 
     * [용도] 대회 목록 썸네일, 상세 페이지 헤더 이미지
     * [기본값] null인 경우 프론트엔드에서 기본 이미지 표시
     * [최적화] CDN을 통한 이미지 서빙 권장
     */
    private String imageUrl;
    
    /**
     * 대회 활성화 상태
     * 
     * [용도] 대회 표시/숨김 제어 (관리자용 정보)
     * [값] true: 표시, false: 숨김 (소프트 삭제)
     * [필터링] Repository에서 isActive=true인 대회만 조회
     */
    private Boolean isActive;
    
    /**
     * 대회가 속한 카테고리 목록
     * 
     * [구조] List<CategoryResponse> - 중첩된 DTO 구조
     * [용도] 대회 분류 정보 표시
     * [활용] 
     * - 카테고리 태그로 표시
     * - 관련 대회 추천시 활용
     * - 카테고리별 필터링 UI
     */
    private List<CategoryResponse> categories;
    
    /**
     * 대회 생성 일시
     * 
     * [용도] 최신 대회 정렬, 등록 순서 확인
     * [활용] "신규" 라벨 표시 기준 (예: 일주일 이내)
     */
    private LocalDateTime createdAt;
    
    /**
     * 대회 수정 일시
     * 
     * [용도] 대회 정보 변경 이력 추적
     * [활용] "업데이트됨" 표시, 변경사항 알림
     */
    private LocalDateTime updatedAt;


    //-----------------수경-----------------------------
    private ContestStatus status;

     // ⭐️⭐️⭐️ 이 부분을 추가해야 합니다! ⭐️⭐️⭐️
    private String organizerEmail;
    private String organizerPhone;
    private String submissionFormat;
    private Integer maxParticipants;
    private List<String> eligibility; // JSONB에서 변환될 필드
    private List<String> tags; // JSONB에서 변환될 필드
    private UUID createdByUserId; // ⭐️ 대회 생성자(등록자)의 ID 필드 ⭐️
    // JSON 문자열을 List<String>으로 파싱하기 위한 ObjectMapper
    // DTO 내부에서 직접 ObjectMapper를 사용하는 것은 상황에 따라 논의될 수 있지만,
    // 현재 구조에서는 간단한 해결책으로 사용할 수 있습니다.
    private static final ObjectMapper objectMapper = new ObjectMapper();

    private String regionSi;
    private String regionGu;



    /**
     * Contest 엔티티를 ContestResponse DTO로 변환하는 정적 팩토리 메서드
     * 
     * [설계 패턴] 정적 팩토리 메서드 패턴
     * [변환 과정]
     * 1. Contest 엔티티의 모든 필드 복사
     * 2. 연관된 Category 엔티티들을 CategoryResponse로 변환
     * 3. Stream API를 활용한 컬렉션 변환
     * 
     * [성능 고려사항]
     * - categories.stream().map(): 각 카테고리마다 CategoryResponse.from() 호출
     * - 카테고리가 많은 경우 성능 영향 있을 수 있음
     * - 필요시 배치 변환 또는 캐싱 적용 고려
     * 
     * [사용 예시]
     * // Service 계층에서
     * Contest contest = contestRepository.findByIdWithCategories(contestId);
     * ContestResponse response = ContestResponse.from(contest);
     * 
     * // 페이징 결과 변환
     * Page<Contest> contests = contestRepository.findByIsActiveTrue(pageable);
     * Page<ContestResponse> responses = contests.map(ContestResponse::from);
     * 
     * [호출 위치]
     * - ContestController.getContests(): 목록 조회시 각 대회마다 호출
     * - ContestController.getContest(): 상세 조회시 단일 호출
     * 
     * [null 안전성]
     * - contest가 null인 경우 NullPointerException 발생
     * - categories가 null인 경우 빈 리스트로 처리됨 (엔티티에서 보장)
     * 
     * [향후 확장]
     * - 사용자별 즐겨찾기 여부 추가: from(Contest contest, boolean isFavorite)
     * - 참가 상태 정보 추가: from(Contest contest, ParticipationStatus status)
     * 
     * @param contest 변환할 Contest 엔티티 (null이면 NullPointerException 발생)
     * @return Contest의 모든 정보가 포함된 ContestResponse 객체
     */
    public static ContestResponse from(Contest contest) {
        // 연관된 카테고리들을 CategoryResponse로 변환
        List<CategoryResponse> categoryResponses = contest.getCategories().stream()
                .map(CategoryResponse::from)  // 각 Category를 CategoryResponse로 변환
                .collect(Collectors.toList());  // List로 수집


         // ⭐️⭐️⭐️ 여기에서 eligibilityList와 tagsList를 먼저 선언하고 초기화합니다. ⭐️⭐️⭐️
        List<String> eligibilityList = Collections.emptyList(); // 빈 리스트로 초기화
        List<String> tagsList = Collections.emptyList();       // 빈 리스트로 초기화

        // eligibility JSON 필드 파싱
        if (contest.getEligibilityJson() != null && !contest.getEligibilityJson().isEmpty()) {
            try {
                eligibilityList = objectMapper.readValue(contest.getEligibilityJson(), new TypeReference<List<String>>() {});
            } catch (JsonProcessingException e) {
                System.err.println("Error parsing eligibility JSON: " + e.getMessage());
                // 오류 처리: 로깅, 기본값 설정 등
            }
        }

        // tags JSON 필드 파싱
        if (contest.getTagsJson() != null && !contest.getTagsJson().isEmpty()) {
            try {
                tagsList = objectMapper.readValue(contest.getTagsJson(), new TypeReference<List<String>>() {});
            } catch (JsonProcessingException e) {
                System.err.println("Error parsing tags JSON: " + e.getMessage());
                // 오류 처리: 로깅, 기본값 설정 등
            }
        }     

        return new ContestResponse(
                contest.getId(),                    // UUID 식별자
                contest.getTitle(),                 // 대회 제목
                contest.getDescription(),           // 대회 설명 (null 가능)
                contest.getOrganizer(),             // 주최자명
                contest.getStartDate(),             // 시작 일시
                contest.getEndDate(),               // 종료 일시
                contest.getRegistrationDeadline(),  // 신청 마감 일시 (null 가능)
                contest.getPrizeDescription(),      // 시상 내역 (null 가능)
                contest.getRequirements(),          // 참가 요건 (null 가능)
                contest.getWebsiteUrl(),            // 웹사이트 URL (null 가능)
                contest.getImageUrl(),              // 이미지 URL (null 가능)
                contest.getIsActive(),              // 활성화 상태
                categoryResponses,                  // 변환된 카테고리 목록
                contest.getCreatedAt(),             // 생성 일시
                contest.getUpdatedAt(),              // 수정 일시
                contest.getStatus(),
                // ⭐️⭐️⭐️ 새로 추가된 필드들입니다. ⭐️⭐️⭐️
                contest.getOrganizerEmail(),
                contest.getOrganizerPhone(),
                contest.getSubmissionFormat(),
                contest.getMaxParticipants(),
                eligibilityList, // 파싱된 List<String>
                tagsList,        // 파싱된 List<String>
                contest.getCreatedByUserId(), // ⭐️ 가장 중요한 필드 매핑 ⭐️
                contest.getRegionSi(),
                contest.getRegionGu()
        );
    }

    //-----------------수경-----------------------------
    

    public static ContestResponse fromEntity(Contest contest) {
        ContestResponse response = new ContestResponse();
        response.setId(contest.getId());
        response.setTitle(contest.getTitle());
        response.setStartDate(contest.getStartDate());
        response.setStatus(contest.getStatus());
        response.setMaxParticipants(contest.getMaxParticipants());
        response.setPrizeDescription(contest.getPrizeDescription());
        response.setRegistrationDeadline(contest.getRegistrationDeadline());

        // 각 Category를 CategoryResponse로 변환
        List<CategoryResponse> categoryResponses = contest.getCategories().stream()
                    .map(CategoryResponse::from)  
                    .collect(Collectors.toList());
        response.setCategories(categoryResponses);

        // 필요에 따라 다른 필드도 추가
        return response;
    }

    //-----------------수경-----------------------------
} 