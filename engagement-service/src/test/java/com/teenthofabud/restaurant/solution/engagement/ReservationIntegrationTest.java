package com.teenthofabud.restaurant.solution.engagement;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
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

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ContextConfiguration(classes = { EngagementServiceApplication.class })
public class ReservationIntegrationTest extends EngagementIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String RESERVATION_URI = "/checkIn/reservation";
    private static final String RESERVATION_URI_BY_ID = "/checkIn/reservation/{id}";
    private static final String RESERVATION_URI_PRIMARY_FILTER = "/checkIn/reservation/primaryFilter";

    private ReservationRepository reservationRepository;

    private ReservationEntity2VoConverter reservationEntity2VoConverter;

    private Integer integrationPort;

    private String reservationDateFormat;

    private String reservationTimeFormat;

    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }

    @Value("${res.engagement.checkIn.reservation.time.format")
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
    private ReservationEntity reservationEntity1;
    private ReservationEntity reservationEntity2;
    private ReservationEntity reservationEntity3;
    private ReservationEntity reservationEntity4;

    private List<PatchOperationForm> patches;

    private AccountVo accountVo(String id, String firstName, String lastName, boolean active) {
        AccountVo accountVo = new AccountVo();
        accountVo.setActive(active);
        accountVo.setId(id);
        accountVo.setFirstName(firstName);
        accountVo.setLastName(lastName);
        return accountVo;
    }

    private ReservationEntity reservationEntity() {
        return null;
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
        reservationForm.setAccountId("1");
        reservationForm.setNotes("New Notes");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "patched accountId"),
                new PatchOperationForm("replace", "/notes", "patched notes"));

        reservationEntity1 = new ReservationEntity();
        reservationEntity1.setAccountId("1");
        reservationEntity1.setNotes("Reservation 1 Notes");
        reservationEntity1.setActive(Boolean.TRUE);

        reservationEntity2 = new ReservationEntity();
        reservationEntity2.setAccountId("2");
        reservationEntity2.setNotes("Reservation 2 Notes");
        reservationEntity2.setActive(Boolean.TRUE);

        reservationEntity3 = new ReservationEntity();
        reservationEntity3.setAccountId("Reservation 3 AccountId");
        reservationEntity3.setNotes("Reservation 3 Notes");
        reservationEntity3.setActive(Boolean.TRUE);

        reservationEntity4 = new ReservationEntity();
        reservationEntity4.setAccountId("Reservation 4 AccountId");
        reservationEntity4.setNotes("Reservation 4 Notes");
        reservationEntity4.setActive(Boolean.FALSE);

        reservationEntity1 = reservationRepository.save(reservationEntity1);

        reservationVo1 = new ReservationVo();
        reservationVo1.setId(reservationEntity1.getId().toString());
        reservationVo1.setAccountId(reservationEntity1.getAccountId());
        reservationVo1.setNotes(reservationEntity1.getNotes());

        reservationEntity2 = reservationRepository.save(reservationEntity2);

        reservationVo2 = new ReservationVo();
        reservationVo2.setId(reservationEntity2.getId().toString());
        reservationVo2.setAccountId(reservationEntity2.getAccountId());
        reservationVo2.setNotes(reservationEntity2.getNotes());

        reservationEntity3 = reservationRepository.save(reservationEntity3);

        reservationVo3 = new ReservationVo();
        reservationVo3.setId(reservationEntity3.getId().toString());
        reservationVo3.setAccountId(reservationEntity3.getAccountId());
        reservationVo3.setNotes(reservationEntity3.getNotes());

        reservationEntity4 = reservationRepository.save(reservationEntity4);

        reservationVo4 = new ReservationVo();
        reservationVo4.setId(reservationEntity4.getId().toString());
        reservationVo4.setAccountId(reservationEntity4.getAccountId());
        reservationVo4.setNotes(reservationEntity4.getNotes());

        reservationVo5 = new ReservationVo();
        reservationVo5.setId(UUID.randomUUID().toString());
        reservationVo5.setAccountId(reservationForm.getAccountId());
        reservationVo5.setNotes(reservationForm.getNotes());

        reservationEntity1 = reservationRepository.save(reservationEntity1);
        reservationEntity2 = reservationRepository.save(reservationEntity2);
        reservationEntity3 = reservationRepository.save(reservationEntity3);
        reservationEntity4 = reservationRepository.save(reservationEntity4);

    }

    @AfterEach
    private void destroy() {
        reservationRepository.deleteById(reservationEntity1.getId());
        reservationRepository.deleteById(reservationEntity2.getId());
        reservationRepository.deleteById(reservationEntity3.getId());
        reservationRepository.deleteById(reservationEntity4.getId());
    }

    @Test
    public void test_Reservation_Post_ShouldReturn_201Response_And_NewReservationId_WhenPosted_WithValidReservationForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(RESERVATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Reservation_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_WithEmptyAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        reservationForm.setAccountId("");

        mvcResult = mockMvc.perform(post(RESERVATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_Reservation_Post_ShouldReturn_201Response_And_NewReservationId_WhenPosted_WithEmptyNotes() throws Exception {
        MvcResult mvcResult = null;
        reservationForm.setAccountId("New Reservation AccountId");
        reservationForm.setNotes("");

        mvcResult = mockMvc.perform(post(RESERVATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Reservation_Post_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenPosted_WithNoReservationForm() throws Exception {
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
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForAllReservations() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>(Arrays.asList(reservationVo1, reservationVo2, reservationVo3, reservationVo4));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Reservation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "filters";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Reservation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequestedBy_EmptyNotesOnly(String notes) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "filters";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("notes", notes))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_EmptyReservationList_WhenRequestedBy_AbsentAccountId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("accountId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_EmptyReservationList_WhenRequestedBy_AbsentNotes() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER).queryParam("notes", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithAccountId() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = new ArrayList<>(Arrays.asList(reservationVo1, reservationVo2, reservationVo3, reservationVo4, reservationVo5));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "Reservation"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithNotes() throws Exception {
        MvcResult mvcResult = null;
        List<ReservationVo> reservationList = new ArrayList<>(Arrays.asList(reservationVo2));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("notes", "Reservation 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_ReservationListNaturallyOrdered_WhenRequested_ForReservations_WithAccountIdAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>(Arrays.asList(reservationVo1));

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "Reservation 1")
                        .queryParam("notes", "Reservation 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_EmptyReservationList_WhenRequested_ForReservations_WithAbsent_WithAccountIdAndNotes() throws Exception {
        MvcResult mvcResult = null;
        Set<ReservationVo> reservationList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_PRIMARY_FILTER)
                        .queryParam("accountId", "Reservation 1")
                        .queryParam("notes", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo[].class).length);
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_ReservationDetails_WhenRequested_ById() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(reservationVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(reservationVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Reservation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
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
        Assertions.assertEquals(reservationVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getId());
        Assertions.assertEquals(reservationVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getAccountId());
        Assertions.assertEquals(reservationVo1.getNotes(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getNotes());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getActive()));
    }

    @Test
    public void test_Reservation_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RESERVATION_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(reservationVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getId());
        Assertions.assertEquals(reservationVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getAccountId());
        Assertions.assertEquals(reservationVo1.getNotes(), om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getNotes());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ReservationVo.class).getActive()));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = reservationEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Delete_ShouldReturn_404Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(delete(RESERVATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndReservationDetails() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        reservationForm.setAccountId("Ferran");

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Reservation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenUpdatedBy_EmptyId_AndReservationDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_404Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenUpdated_ByAbsentId_AndReservationDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_005_WhenUpdated_ByInactiveId_AndReservationDetails() throws Exception {
        String id = reservationEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenUpdated_ById_AndNoReservationDetails() throws Exception {
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
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidAccountId() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "accountId";
        reservationForm.setAccountId("");

        mvcResult = mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidNotes() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "notes";
        reservationForm.setNotes("");

        mvcResult = mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(reservationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenUpdated_ById_AndEmptyReservationDetails() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldAccountId = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(RESERVATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new ReservationForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndReservationDetails() throws Exception {
        String id = reservationEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenUpdated_ByEmptyId_AndReservationDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(patch(RESERVATION_URI_BY_ID, " ")
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_404Response_And_ErrorCode_RES_ENGAGEMENT_002_WhenUpdated_ByAbsentId_AndReservationDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldAccountId = "id";

        mvcResult = this.mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_422Response_And_ErrorCode_RES_ENGAGEMENT_003_WhenUpdated_ById_AndNoReservationDetails() throws Exception {
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
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

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
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

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
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

    }

    @Test
    public void test_Reservation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGAGEMENT_001_WhenRequested_ById_AndInvalidDefinitionOfReservationAttribute() throws Exception {
        String id = reservationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldAccountId = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(RESERVATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldAccountId));

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
