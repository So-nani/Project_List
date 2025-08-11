package com.contestapp.contestservice.service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.contestapp.contestservice.dto.request.ContestCreateRequest;
import com.contestapp.contestservice.dto.request.ContestUpdateRequest;
import com.contestapp.contestservice.dto.response.ContestResponse;
import com.contestapp.contestservice.entity.Category;
import com.contestapp.contestservice.entity.Contest;
import com.contestapp.contestservice.entity.ContestStatus;
import com.contestapp.contestservice.repository.CategoryRepository;
import com.contestapp.contestservice.repository.ContestRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;

/**
 * Contest 비즈니스 로직 처리 서비스
 * 
 * [역할]
 * - 대회 관련 핵심 비즈니스 로직 구현
 * - 컨트롤러와 리포지토리 사이의 중간 계층
 * - 트랜잭션 관리 및 데이터 검증
 * 
 * [서비스 계층의 책임]
 * 1. 비즈니스 규칙 검증 (예: 대회 날짜 유효성)
 * 2. 복잡한 조회 로직 처리 (필터링, 정렬)
 * 3. 여러 리포지토리를 조합한 복합 작업
 * 4. 트랜잭션 경계 설정
 * 
 * [설계 패턴]
 * - DI (Dependency Injection): @RequiredArgsConstructor로 의존성 주입
 * - 읽기 전용 트랜잭션: @Transactional(readOnly = true)로 성능 최적화
 * 
 * [팀원 확장 가이드]
 * 1. 새로운 비즈니스 로직 추가시 이 클래스에 메서드 추가
 * 2. 데이터 변경 작업시 @Transactional 어노테이션 필수
 * 3. 예외 처리는 명확한 메시지와 함께 IllegalArgumentException 사용
 * 4. 복잡한 검증 로직은 별도 Validator 클래스로 분리 고려
 */
@Service  // Spring의 비즈니스 로직 계층 컴포넌트로 등록
@RequiredArgsConstructor  // Lombok: final 필드들을 파라미터로 받는 생성자 자동 생성
@Transactional(readOnly = true)  // 클래스 레벨: 모든 메서드에 읽기 전용 트랜잭션 적용
public class ContestService {

    private static final Logger log = LoggerFactory.getLogger(ContestService.class);

    /**
     * Contest 데이터 접근 객체
     * 
     * [의존성 주입] @RequiredArgsConstructor에 의해 생성자 주입
     * [사용 목적] 데이터베이스 조회/저장 작업 위임
     */
    private final ContestRepository contestRepository;
    private final CategoryRepository categoryRepository;
    //private final ContestCategoriesRepository contestCategoriesRepository;
    private final ObjectMapper objectMapper;


    /**
     * 대회 목록 조회 (다양한 필터링 조건 지원)
     * 
     * [비즈니스 로직]
     * 1. 키워드 검색 우선순위 최고 (제목, 설명, 주최자 대상)
     * 2. 카테고리 필터링 (특정 카테고리의 대회만)
     * 3. 기본 조회 (활성화된 모든 대회)
     * 
     * [필터링 우선순위]
     * keyword > categoryId > 전체 목록
     * 
     * [성능 최적화]
     * - 읽기 전용 트랜잭션으로 DB 최적화
     * - 페이징 처리로 메모리 사용량 제한
     * 
     * [사용 예시]
     * // 전체 대회 목록
     * Page<Contest> allContests = contestService.findContests(null, null, pageable);
     * 
     * // 카테고리 필터링
     * Page<Contest> programmingContests = contestService.findContests(1L, null, pageable);
     * 
     * // 키워드 검색
     * Page<Contest> searchResults = contestService.findContests(null, "프로그래밍", pageable);
     * 
     * // 키워드가 있으면 카테고리 무시됨
     * Page<Contest> keywordOnly = contestService.findContests(1L, "AI", pageable); // 카테고리 1L 무시
     * 
     * [컨트롤러 연결] ContestController.getContests()에서 호출
     * 
     * [향후 확장 고려사항]
     * 1. 정렬 조건 추가 (인기순, 마감일순)
     * 2. 복합 필터링 (카테고리 + 키워드 동시 적용)
     * 3. 대회 상태별 필터링 (진행중, 예정, 종료)
     * 4. 지역별, 주최자별 필터링
     * 
     * @param categoryId 카테고리 ID (null이면 카테고리 필터링 무시)
     * @param keyword 검색 키워드 (null이거나 빈 문자열이면 검색 무시)
     * @param pageable 페이징 정보 (페이지 번호, 크기, 정렬 조건)
     * @return 필터링된 대회 목록 (페이징 정보 포함)
     */
    public Page<Contest> findContests(Long categoryId, String keyword, Pageable pageable) {
        // 키워드 검색이 최우선 - 키워드가 있으면 다른 조건 무시
        if (keyword != null && !keyword.trim().isEmpty()) {
            // 앞뒤 공백 제거 후 키워드 검색 실행
            return contestRepository.findByKeyword(keyword.trim(), pageable);
        }
        
        // 카테고리 필터링 - 키워드가 없고 카테고리 ID가 있는 경우
        if (categoryId != null) {
            return contestRepository.findByCategoryId(categoryId, pageable);
        }
        
        // 기본 조회 - 필터링 조건이 없으면 활성화된 모든 대회 조회
        return contestRepository.findByIsActiveTrue(pageable);
    }

    /**
     * 대회 상세 조회 (카테고리 정보 포함)
     * 
     * [비즈니스 로직]
     * 1. 대회 존재 여부 확인
     * 2. 활성화 상태 확인 (isActive = true)
     * 3. 연관된 카테고리 정보 함께 조회 (N+1 문제 방지)
     * 
     * [성능 최적화]
     * - FETCH JOIN으로 카테고리 정보 한 번에 로딩
     * - 읽기 전용 트랜잭션으로 DB 성능 향상
     * 
     * [예외 처리]
     * - 대회를 찾을 수 없는 경우: IllegalArgumentException
     * - 비활성화된 대회도 찾을 수 없음으로 처리
     * 
     * [사용 예시]
     * try {
     *     Contest contest = contestService.findById(contestId);
     *     // contest.getCategories() 호출해도 추가 쿼리 발생 안함
     *     List<Category> categories = contest.getCategories();
     * } catch (IllegalArgumentException e) {
     *     // 대회를 찾을 수 없는 경우 처리
     *     return ResponseEntity.notFound().build();
     * }
     * 
     * [컨트롤러 연결] ContestController.getContest()에서 호출
     * 
     * [보안 고려사항]
     * - 비활성화된 대회는 조회 불가 (관리자만 접근 가능하도록 별도 메서드 필요)
     * - UUID 사용으로 대회 ID 추측 공격 방지
     * 
     * @param contestId 조회할 대회의 UUID
     * @return 카테고리 정보가 포함된 Contest 엔티티
     * @throws IllegalArgumentException 대회를 찾을 수 없거나 비활성화된 경우
     */
    public Contest findById(UUID contestId) {
        return contestRepository.findByIdWithCategories(contestId)
                .orElseThrow(() -> new IllegalArgumentException("Contest not found with ID: " + contestId));
    }

    /**
     * 대회 존재 여부 확인
     * 
     * [목적] 
     * - 다른 서비스에서 대회 유효성 검증용
     * - 참가 신청, 즐겨찾기 등 기능에서 사전 검증
     * 
     * [성능 특징]
     * - 전체 엔티티 로딩 없이 존재 여부만 확인
     * - COUNT 쿼리로 빠른 응답
     * - 읽기 전용 트랜잭션으로 최적화
     * 
     * [사용 예시]
     * // FavoriteService에서 대회 유효성 검증
     * if (!contestService.existsById(contestId)) {
     *     throw new IllegalArgumentException("Contest not found");
     * }
     * 
     * // ParticipationService에서 참가 신청 전 검증
     * boolean isValidContest = contestService.existsById(contestId);
     * 
     * [주의사항]
     * - 이 메서드는 비활성화된 대회도 true 반환
     * - 활성화 상태까지 확인이 필요하면 findById() 사용 권장
     * 
     * [향후 개선]
     * - existsByIdAndIsActiveTrue() 메서드 추가 고려
     * - 캐싱 적용으로 성능 향상 가능
     * 
     * @param contestId 확인할 대회의 UUID
     * @return 대회 존재 여부 (true: 존재, false: 없음)
     */
    public boolean existsById(UUID contestId) {
        return contestRepository.existsById(contestId);
    }

    /*
     * [향후 확장 예정 메서드들]
     * 
     * 1. 대회 생성
     * @Transactional
     * public Contest createContest(ContestCreateRequest request) {
     *     // 비즈니스 규칙 검증 (날짜 유효성, 카테고리 존재 여부 등)
     *     // Contest 엔티티 생성 및 저장
     * }
     * 
     * 2. 대회 수정
     * @Transactional
     * public Contest updateContest(UUID contestId, ContestUpdateRequest request) {
     *     // 대회 존재 확인, 수정 권한 검증
     *     // 변경사항 적용 및 저장
     * }
     * 
     * 3. 대회 비활성화 (소프트 삭제)
     * @Transactional
     * public void deactivateContest(UUID contestId) {
     *     // 대회 존재 확인, 삭제 권한 검증
     *     // isActive = false로 변경
     * }
     * 
     * 4. 인기 대회 조회
     * public Page<Contest> findPopularContests(Pageable pageable) {
     *     // 참가자 수, 즐겨찾기 수 기준으로 정렬
     * }
     * 
     * 5. 대회 상태별 조회
     * public Page<Contest> findContestsByStatus(ContestStatus status, Pageable pageable) {
     *     // 진행중, 예정, 종료 상태별 조회
     * }
     */

  // 1. 대회 생성 (createContest)
    @Transactional
    public Contest createContest(ContestCreateRequest request, UUID creatorUserId) {
        // ... (날짜 유효성 검사 등 기존 로직) ...

        String eligibilityJsonString = null;
        String tagsJsonString = null;

        List<Long> categoryIds = request.getCategoryIds(); // 클라이언트로부터 전달된 카테고리 ID 리스트
        List<Category> categories = categoryRepository.findAllById(categoryIds);

        if (categories.size() != categoryIds.size()) {
            throw new IllegalArgumentException("유효하지 않은 카테고리 ID가 포함되어 있습니다.");
        }

        try {
            // List<String>을 유효한 JSON 배열 문자열로 변환합니다.
            // request.getEligibility()가 null이 아니면 변환 시도
            if (request.getEligibility() != null) {
                eligibilityJsonString = objectMapper.writeValueAsString(request.getEligibility());
            }
            // request.getTags()가 null이 아니면 변환 시도
            if (request.getTags() != null) {
                tagsJsonString = objectMapper.writeValueAsString(request.getTags());
            }
        } catch (JsonProcessingException e) {
            // JSON 변환 중 오류가 발생하면 로그를 남기고 예외를 다시 발생시킵니다.
            log.error("참가 자격 또는 태그 목록을 JSON 문자열로 변환하는 데 실패했습니다.", e);
            throw new RuntimeException("대회 자격/태그 JSON 데이터 처리 중 오류 발생", e);
        }

        ContestStatus initialStatus = calculateStatus(request.getEndDate());

        // Contest 엔티티 생성
        Contest contest = Contest.builder()
                .title(request.getTitle())
                .description(request.getDescription())
                .organizer(request.getOrganizer())
                .startDate(request.getStartDate())
                .endDate(request.getEndDate())
                .registrationDeadline(request.getRegistrationDeadline())
                .prizeDescription(request.getPrizeDescription())
                .requirements(request.getRequirements())
                .websiteUrl(request.getWebsiteUrl())
                .imageUrl(request.getImageUrl())
                .isActive(true)
                .organizerEmail(request.getOrganizerEmail())
                .organizerPhone(request.getOrganizerPhone())
                .submissionFormat(request.getSubmissionFormat())
                .maxParticipants(request.getMaxParticipants())
                // **여기서 올바르게 포맷된 JSON 문자열을 사용합니다.**
                .eligibilityJson(eligibilityJsonString)
                .tagsJson(tagsJsonString)
                .createdByUserId(creatorUserId)// 실제 사용자 ID로 변경해야 합니다.B
                .categories(categories)  // ✅ ManyToMany로 연결
                .status(initialStatus) // ✅ 현재 시간 기준으로 상태 설정
                .regionSi(request.getRegionSi())
                .regionGu(request.getRegionGu())
                .build();
        
        // contest.setCategories(categories);  // @ManyToMany 관계에 카테고리 설정
        // 대회 저장
        return contestRepository.save(contest);
    }

// 2. 대회 수정 (updateContest)
    @Transactional
    public Contest updateContest(UUID contestId, ContestUpdateRequest request) {
        log.info("Updating contest with ID: {}", contestId);
        Contest contest = contestRepository.findById(contestId)
                .orElseThrow(() -> new IllegalArgumentException("Contest not found with ID: " + contestId));

        // ... (권한 검증 로직은 주석 유지 또는 구현 필요)

        // 모든 필드를 request가 null이 아닐 때만 업데이트
        if (request.getTitle() != null) {
            contest.setTitle(request.getTitle());
        }
        if (request.getDescription() != null) {
            contest.setDescription(request.getDescription());
        }
        if (request.getOrganizer() != null) {
            contest.setOrganizer(request.getOrganizer());
        }
        if (request.getPrizeDescription() != null) {
            contest.setPrizeDescription(request.getPrizeDescription());
        }
        if (request.getRequirements() != null) {
            contest.setRequirements(request.getRequirements());
        }
        if (request.getWebsiteUrl() != null) {
            contest.setWebsiteUrl(request.getWebsiteUrl());
        }
        if (request.getImageUrl() != null) {
            contest.setImageUrl(request.getImageUrl());
        }
        if (request.getOrganizerEmail() != null) {
            contest.setOrganizerEmail(request.getOrganizerEmail());
        }
        if (request.getOrganizerPhone() != null) {
            contest.setOrganizerPhone(request.getOrganizerPhone());
        }
        if (request.getSubmissionFormat() != null) {
            contest.setSubmissionFormat(request.getSubmissionFormat());
        }
        if (request.getMaxParticipants() != null) {
            contest.setMaxParticipants(request.getMaxParticipants());
        }

        // 날짜 필드 처리 (LocalDateTime 타입 그대로 사용)
        if (request.getStartDate() != null) {
            contest.setStartDate(request.getStartDate());
        }
        if (request.getEndDate() != null) {
            contest.setEndDate(request.getEndDate());
        }
        if (request.getRegistrationDeadline() != null) {
            contest.setRegistrationDeadline(request.getRegistrationDeadline());
        }

        if (request.getIsActive() != null) {
            contest.setIsActive(request.getIsActive());
        }

        // --- 카테고리 업데이트 처리 ---
        if (request.getCategoryIds() != null) {
            List<Long> categoryIds = request.getCategoryIds().stream()
                                            .map(ContestUpdateRequest.CategoryIdDto::getId)
                                            .collect(Collectors.toList());
            List<Category> categories = categoryRepository.findAllById(categoryIds);
            if (categories.size() != categoryIds.size()) {
                throw new IllegalArgumentException("유효하지 않은 카테고리 ID가 포함되어 있습니다.");
            }
            contest.setCategories(categories);
        }
        // --- 카테고리 업데이트 처리 끝 ---

        // --- JSONB 필드 처리: 핵심 수정 부분 ---
        try {
            // eligibilityList가 null이 아니면 JSON 문자열로 변환
            if (request.getEligibility() != null) {
                // List가 비어있을 경우 "[]"로, 리스트에 요소가 있으면 "[\"값1\",\"값2\"]"로 변환
                contest.setEligibilityJson(objectMapper.writeValueAsString(request.getEligibility()));
            } else {
                // request.getEligibility()가 null일 경우, DB 컬럼에 null을 저장하고 싶으면 아래처럼.
                // 만약 빈 배열("[]")을 저장하고 싶다면 contest.setEligibilityJson("[]"); 또는
                // contest.setEligibilityJson(objectMapper.writeValueAsString(Collections.emptyList()));
                contest.setEligibilityJson(null); // 혹은 기존 값 유지 로직
            }

            // tagsList도 위와 동일하게 처리
            if (request.getTags() != null) {
                contest.setTagsJson(objectMapper.writeValueAsString(request.getTags()));
            } else {
                // request.getTags()가 null일 경우
                contest.setTagsJson(null); // 혹은 기존 값 유지 로직
            }

        } catch (JsonProcessingException e) {
            log.error("JSON 데이터를 문자열로 변환하는 중 오류 발생: {}", e.getMessage(), e);
            throw new RuntimeException("대회 자격/태그 데이터 처리 중 오류가 발생했습니다.", e);
        }
        // --- JSONB 필드 처리 끝 ---

        // 주최자 ID 설정 (예시로 UUID 생성, 실제로는 현재 로그인한 사용자 ID로 설정해야 함)
        // contest.setCreatedByUserId(UUID.randomUUID()); // 업데이트 시에는 보통 생성자 ID를 바꾸지 않습니다.
                                                     // 이 부분은 생성 시에만 적용하고 업데이트에서는 제거하거나,
                                                     // 필요하다면 request.getCreatedByUserId() 등으로 받아서 업데이트하세요.

        // 업데이트 시간 자동 갱신 (JPA @LastModifiedDate 사용 시 필요 없음)
        // contest.setUpdatedAt(LocalDateTime.now());

        // 대회 저장 (변경된 필드들이 DB에 반영됨)
        Contest updatedContest = contestRepository.save(contest);
        log.info("Contest with ID: {} updated successfully.", contestId);

        return updatedContest;
    }

        
    // 3. 대회 비활성화 (소프트 삭제)
    @Transactional
    public void deactivateContest(UUID contestId) {
        log.info("대회 ID: {} 비활성화 시도 중.", contestId);

        // 1. 대회 존재 확인
        Contest contest = contestRepository.findById(contestId)
                .orElseThrow(() -> new IllegalArgumentException("ID: " + contestId + "에 해당하는 대회를 찾을 수 없습니다."));

        // 2. isActive를 false로 설정하여 비활성화
        contest.setIsActive(false);

        // 3. 변경된 상태를 저장 (소프트 삭제)
        contestRepository.save(contest); // <--- 이 부분을 delete 대신 save로 변경!

        log.info("대회 ID: {}가 성공적으로 비활성화되었습니다.", contestId);
    }

    

   
    // // 4. 인기 대회 조회
    //   public Page<Contest> findPopularContests(Pageable pageable) {
    //       // 참가자 수, 즐겨찾기 수 기준으로 정렬
    //       return contestRepository.findPopularContests(pageable); }


    // // 5. 대회 상태별 조회
    // public Page<Contest> findContestsByStatus(LocalDateTime now, Pageable pageable) {
    //     // 진행중, 예정, 종료 상태별 조회
    //     return contestRepository.findOngoingContests(now, pageable);
    // }   
    // // 6. 특정 사용자의 즐겨찾기 대회 조회
    // public Page<Contest> findFavoriteContestsByUserId(UUID userId, Pageable pageable) {
    //     // 사용자의 즐겨찾기 대회 조회
    //     return contestRepository.findFavoriteContestsByUserId(userId, pageable);
    // }


    //----------------------------수경--------------------------
    // 3일을 기준으로 임박 설정. 현재를 기준으로 마감인지 설정.
    public ContestStatus calculateStatus(LocalDateTime endDate) {
        LocalDateTime now = LocalDateTime.now();

        if (endDate.isBefore(now)) {        //endDate가 현재보다 뒤에 있나? -> 이미 마감일이 끝났다.
            return ContestStatus.CLOSED;
        } else if (endDate.minusDays(3).isBefore(now)) {
            return ContestStatus.CLOSING_SOON;
        } else {
            return ContestStatus.OPEN;
        }
    }



    // @Transactional
    // private ContestCategories contestCategoryUpdate(UUID userId, UUID contest_id, int carteroyId) {
        
    //     // Contest 엔티티 생성
    //     ContestCategories contest = ContestCategories.builder()
    //     .userId(userId)
    //     .contestId(contest_id)
    //     .categoryId(carteroyId)                
    //     .build();

    //     return contestCategoriesRepository.save(contest);
        
    // }


    public Page<ContestResponse> getContests(ContestStatus status, int page, int size, String sortBy, String sortDir) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<Contest> contests;

        if (status == null) {
            // 전체 목록 조회
            contests = contestRepository.findAll(pageable);
        } else {
            // 상태별 조회
            contests = contestRepository.findByStatus(status, pageable);
        }

        return contests.map(ContestResponse::fromEntity);
    } 

    //자동으로 업데이트 
    // @Scheduled(fixedDelay = 10000)      //빠른 결과 확인을 위해 실행
    //@Scheduled(cron = "0 0 * * * *") // 매 시간마다 실행
    @Scheduled(fixedDelay = 10000)
    public void updateContestStatuses() {
        System.out.println(" 스케줄러 실행됨!");
        List<Contest> contests = contestRepository.findAll();
        for (Contest contest : contests) {
            ContestStatus newStatus = calculateStatus(contest.getEndDate());
             if (contest.getStatus() != newStatus) {
            log.info("📌 상태 변경됨: ID={}, {} → {}", contest.getId(), contest.getStatus(), newStatus);
            contest.setStatus(newStatus);
            }
        }
         try {
        contestRepository.saveAll(contests);
        } catch (Exception e) {
            log.error("❌ Contest 저장 중 오류 발생", e);
        }
    }

    @PostConstruct
    public void init() {
        updateContestStatuses();  // 서버 시작 직후 자동 실행
    }

    public Page<Contest> findContestsByRegion(String regionSi, String regionGu, Pageable pageable) {
        if (regionSi != null && regionGu != null) {
            return contestRepository.findByRegionSiAndRegionGu(regionSi, regionGu, pageable);
        } else if (regionSi != null) {
            return contestRepository.findByRegionSi(regionSi, pageable);
        } else {
            return contestRepository.findAll(pageable);
        }
    }



    //----------------------------수경--------------------------




 }