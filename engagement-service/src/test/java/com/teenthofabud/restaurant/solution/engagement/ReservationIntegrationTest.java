package com.teenthofabud.restaurant.solution.engagement;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.engagement.checkin.constants.CheckInType;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.ReservationRepository;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ContextConfiguration(classes = { EngagementServiceApplication.class })
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class ReservationIntegrationTest extends EngagementIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";
    private static final String RESERVATION_URI = "/checkIn/reservation";
    private static final String RESERVATION_URI_BY_ID = String.join("/", RESERVATION_URI, "{id}");
    private static final String RESERVATION_URI_BY_SEQUENCE = String.join("/", RESERVATION_URI, "sequence", "{sequence}");
    private static final String RESERVATION_URI_PRIMARY_FILTER = String.join("/", RESERVATION_URI, "primaryFilter");
    private static final String RESERVATION_URI_SECONDARY_FILTER = String.join("/", RESERVATION_URI, "secondaryFilter");

    private ReservationRepository reservationRepository;

    private ReservationEntity2VoConverter reservationEntity2VoConverter;

    private Integer integrationPort;

    private String reservationDateFormat;

    private String reservationTimeFormat;

    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }

    @Value("${res.engagement.checkIn.reservation.time.format}")
    public void setReservationTimeFormat(String reservationTimeFormat) {
        this.reservationTimeFormat = reservationTimeFormat;
    }

    @Value("${res.engagement.integration.gateway.port}")
    public void setIntegrationPort(Integer integrationPort) {
        this.integrationPort = integrationPort;
    }

    @Autowired
    public void setReservationRepository(ReservationRepository reservationRepository) {
        this.reservationRepository = reservationRepository;
    }

    @Autowired
    public void setReservationEntity2VoConverter(ReservationEntity2VoConverter reservationEntity2VoConverter) {
        this.reservationEntity2VoConverter = reservationEntity2VoConverter;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo22;
    private AccountVo accountVo4;

    private ReservationForm reservationForm;
    private ReservationVo reservationVo1;
    private ReservationVo reservationVo2;
    private ReservationVo reservationVo3;
    private ReservationVo reservationVo4;
    private ReservationVo reservationVo5;
    private ReservationVo reservationVo6;
    private ReservationEntity reservationEntity1;
    private ReservationEntity reservationEntity2;
    private ReservationEntity reservationEntity3;
    private ReservationEntity reservationEntity4;
    private ReservationEntity reservationEntity5;

    private List<PatchOperationForm> patches;

    private AccountVo accountVo(String id, String firstName, String lastName, boolean active) {
        AccountVo accountVo = new AccountVo();
        accountVo.setActive(active);
        accountVo.setId(id);
        accountVo.setFirstName(firstName);
        accountVo.setLastName(lastName);
        return accountVo;
    }

    private ReservationEntity reservationEntity(String accountId, String sequence, Integer noOfPersons, String notes, boolean active, LocalDate date, LocalTime time) {
        ReservationEntity reservationEntity = new ReservationEntity();
        reservationEntity.setTime(time);
        reservationEntity.setSequence(sequence);
        reservationEntity.setDate(date);
        reservationEntity.setNotes(notes);
        reservationEntity.setNoOfPersons(noOfPersons);
        reservationEntity.setAccountId(accountId);
        reservationEntity.setActive(active);
        return reservationEntity;
    }

    @BeforeEach
    private void init() {

        /**
         * Account
         */

        accountVo1 = this.accountVo("1", "Account 1", "Account 1", true);
        accountVo2 = this.accountVo("2", "Account 2", "Account 2", true);
        accountVo22 = this.accountVo("22", "Account 22", "Account 22", false);
        accountVo4 = this.accountVo("4", "Account 4", "Account 4", true);

        /**
         * Reservation
         */

        reservationForm = new ReservationForm();
        reservationForm.setAccountId(accountVo4.getId());
        reservationForm.setNotes("New Notes");
        reservationForm.setTime(DateTimeFormatter.ofPattern(reservationTimeFormat).format(LocalTime.now()));
        reservationForm.setDate(DateTimeFormatter.ofPattern(reservationDateFormat).format(LocalDate.now()));
        reservationForm.setSequence(UUID.randomUUID().toString());
        reservationForm.setNoOfPersons(45);
        reservationForm.setType(CheckInType.RESERVATION.name());

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "4"),
                new PatchOperationForm("replace", "/notes", "patched notes"));

        reservationEntity1 = this.reservationEntity(accountVo1.getId(), UUID.randomUUID().toString(), 2, "reservation 1 notes", true, LocalDate.now().plusDays(1), LocalTime.now().plusHours(1));
        reservationEntity1 = reservationRepository.save(reservationEntity1);
        reservationVo1 = this.reservationEntity2VoConverter.convert(reservationEntity1);

        reservationEntity2 = this.reservationEntity(accountVo2.getId(), UUID.randomUUID().toString(), 21, "reservation 2 notes", true, LocalDate.now().plusDays(2), LocalTime.now().plusHours(2));
        reservationEntity2 = reservationRepository.save(reservationEntity2);
        reservationVo2 = this.reservationEntity2VoConverter.convert(reservationEntity2);

        reservationEntity3 = this.reservationEntity(accountVo1.getId(), UUID.randomUUID().toString(), 28, "reservation 3 notes", true, LocalDate.now().plusDays(3), LocalTime.now().plusHours(3));
        reservationEntity3 = reservationRepository.save(reservationEntity3);
        reservationVo3 = this.reservationEntity2VoConverter.convert(reservationEntity3);

        reservationEntity4 = this.reservationEntity(accountVo22.getId(), UUID.randomUUID().toString(), 12, "reservation 4 notes", false, LocalDate.now().plusDays(4), LocalTime.now().plusHours(4));
        reservationEntity4 = reservationRepository.save(reservationEntity4);
        reservationVo4 = this.reservationEntity2VoConverter.convert(reservationEntity4);

        reservationEntity5 = this.reservationEntity(accountVo4.getId(), UUID.randomUUID().toString(), 244, "reservation 5 notes", true, LocalDate.now().plusDays(5), LocalTime.now().plusHours(5));
        reservationEntity5 = reservationRepository.save(reservationEntity5);
        reservationVo5 = this.reservationEntity2VoConverter.convert(reservationEntity5);

        reservationVo6 = new ReservationVo();
        reservationVo6.setActive(true);
        reservationVo6.setSequence(reservationForm.getSequence());
        reservationVo6.setCreatedOn(LocalDateTime.now());
        reservationVo6.setNotes(reservationForm.getNotes());
        reservationVo6.setNoOfPersons(reservationForm.getNoOfPersons());
        reservationVo6.setAccount(accountVo4);
        reservationVo6.setType(reservationForm.getType());
        reservationVo6.setTime(reservationForm.getTime());
        reservationVo6.setDate(reservationForm.getDate());
        reservationVo6.setId("6");

    }

    @AfterEach
    private void destroy() {
        reservationRepository.deleteById(reservationEntity1.getId());
        reservationRepository.deleteById(reservationEntity2.getId());
        reservationRepository.deleteById(reservationEntity3.getId());
        reservationRepository.deleteById(reservationEntity4.getId());
        reservationRepository.deleteById(reservationEntity5.getId());
    }

    @Test
    public void test_Reservation_Post_ShouldReturn_201Response_And_NewReservationId_WhenPosted_WithValidReservationForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(RESERVATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Reservation_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        reservationForm.setAccountId("");

        mvcResult = mockMvc.perform(post(RESERVATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_Reservation_Post_ShouldReturn_201Response_And_NewReservationId_WhenPosted_WithEmptyNotes() throws Exception {
        MvcResult mvcResult = null;
        reservationForm.setAccountId("2");
        reservationForm.setNotes("");

        mvcResult = mockMvc.perform(post(RESERVATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Reservation_Post_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenPosted_WithNoReservationForm() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(RESERVATION_URI)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForAllReservations() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = Arrays.asList(reservationVo3, reservationVo1, reservationVo2, reservationVo4, reservationVo5);

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Reservation_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Reservation_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyNotesOnly(String notes) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("notes", notes))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Reservation_Get_Primary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptySequenceOnly(String sequence) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String field = "filters";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("sequence", sequence))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field));
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequestedBy_AbsentAccountId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("accountId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequestedBy_AbsentNotes() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("notes", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequestedBy_AbsentSequence() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("sequence", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithAccountId() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = new ArrayList<>(Arrays.asList(reservationVo5));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithNotes() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = new ArrayList<>(Arrays.asList(reservationVo2));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("notes", "reservation 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithSequence() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = new ArrayList<>(Arrays.asList(reservationVo2));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("sequence",  reservationVo2.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithAccountIdAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>(Arrays.asList(reservationVo1));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", reservationVo1.getAccountId())
                        .queryParam("notes", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithAccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>(Arrays.asList(reservationVo1));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", reservationVo1.getAccountId())
                        .queryParam("sequence", reservationVo1.getSequence()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>(Arrays.asList(reservationVo1));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("sequence", reservationVo1.getSequence())
                        .queryParam("notes", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithAccountIdAndSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>(Arrays.asList(reservationVo1));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", reservationVo1.getAccountId())
                        .queryParam("sequence", reservationVo1.getSequence())
                        .queryParam("notes", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequested_ForReservations_WithAbsent_WithAccountIdAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("notes", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequested_ForReservations_WithAbsent_WithAccountIdAndSequence() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("sequence", "Reservation 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequested_ForReservations_WithAbsent_WithSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("notes", "5 notes")
                        .queryParam("sequence", "Reservation 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Primary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequested_ForReservations_WithAbsent_WithAccountIdAndSequenceAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "4")
                        .queryParam("notes", UUID.randomUUID().toString())
                        .queryParam("sequence", "1 notes"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    /**
     *
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Reservation_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyDateOnly(String date) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "filters";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("date", date))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Reservation_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyTimeOnly(String time) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldTime = "filters";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("time", time))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldTime));
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_InvalidDate() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "date";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("date", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequestedBy_AbsentDate() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("date", "12-09-1976"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_InvalidTime() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldDate = "time";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("time", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldDate));
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequestedBy_AbsentTime() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("time", "06:00"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithDate() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = new ArrayList<>(Arrays.asList(reservationVo5));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("date", reservationVo5.getDate()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithTime() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = new ArrayList<>(Arrays.asList(reservationVo2));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER).queryParam("time", reservationVo2.getTime()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithDateAndTime() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>(Arrays.asList(reservationVo1));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER)
                        .queryParam("date", reservationVo1.getDate())
                        .queryParam("time", reservationVo1.getTime()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_Secondary_ShouldReturn_200Response_And_EmptyReservationList_WhenRequested_ForReservations_WithAbsent_WithDateAndTime() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_SECONDARY_FILTER)
                        .queryParam("date", "22-06-2006")
                        .queryParam("time", "15:45"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    /**
     *
     */

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_ReservationDetails_WhenRequested_ById() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(objectMapper.writeValueAsString(reservationVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(reservationVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Reservation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = reservationEntity3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getId());
        Assertions.assertEquals(reservationVo1.getAccountId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getAccountId());
        Assertions.assertEquals(reservationVo1.getNotes(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getNotes());
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getActive()));
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        reservationVo1.setAccount(accountVo1);

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getId());
        Assertions.assertNull(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getAccountId());
        Assertions.assertEquals(reservationVo1.getAccount(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getAccount());
        Assertions.assertEquals(reservationVo1.getNotes(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getNotes());
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getActive()));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = reservationEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndReservationDetails() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        reservationForm.setAccountId(accountVo4.getId());

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Reservation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedBy_EmptyId_AndReservationDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenUpdated_ByAbsentId_AndReservationDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_005_WhenUpdated_ByInactiveId_AndReservationDetails() throws Exception {
        String id = reservationEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdated_ById_AndNoReservationDetails() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        reservationForm.setAccountId("");

        mvcResult = mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidNotes() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "notes";
        reservationForm.setNotes("");

        mvcResult = mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdated_ById_AndEmptyReservationDetails() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(new ReservationForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndReservationDetails() throws Exception {
        String id = reservationEntity4.getId().toString();
        MvcResult mvcResult = null;
        /*patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "5"),
                new PatchOperationForm("replace", "/notes", "patched notes"));*/

        mvcResult = this.mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdated_ByEmptyId_AndReservationDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(patch(RESERVATION_URI_BY_ID, " ")
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenUpdated_ByAbsentId_AndReservationDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"));

        mvcResult = this.mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdated_ById_AndNoReservationDetails() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", " "));

        mvcResult = mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyRate() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/rate", " "));

        mvcResult = mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidDefinitionOfReservationAttribute() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Override
    public String getSimulationBaseLocation() {
        return "simulation/customer-service";
    }

    @Override
    public Integer getServicePort() throws UnsupportedOperationException {
        return integrationPort;
    }

    @Override
    public String[] getSimulationFilePaths() {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation-v3.json") };
    }
}
