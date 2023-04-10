package com.teenthofabud.restaurant.solution.engagement.tableallocation;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.engagement.EngagementIntegrationBaseTest;
import com.teenthofabud.restaurant.solution.engagement.EngagementServiceApplication;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.ReservationRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.WalkInRepository;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.engagement.integration.establishmentarea.data.TableVo;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.converter.TableAllocationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationForm;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationVo;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.repository.TableAllocationRepository;
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
import java.time.LocalTime;
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
public class TableAllocationIntegrationTest extends EngagementIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String TABLE_ALLOCATION_URI = "/tableAllocation";
    private static final String TABLE_ALLOCATION_URI_BY_ID = "/tableAllocation/{id}";
    private static final String TABLE_ALLOCATION_URI_BY_CHECKIN_ID = "/tableAllocation/checkInId/{checkInId}";
    private static final String TABLE_ALLOCATION_URI_FILTER = "/tableAllocation/filter";

    private TableAllocationRepository tableAllocationRepository;
    private ReservationRepository reservationRepository;
    private WalkInRepository walkInRepository;

    private TableAllocationEntity2VoConverter tableAllocationEntity2VoConverter;
    private ReservationEntity2VoConverter reservationEntity2VoConverter;

    private WalkInEntity2VoConverter walkInEntity2VoConverter;

    private int integrationServicePort;

    @Value("${res.engagement.integration.gateway.port}")
    public void setIntegrationServicePort(int integrationServicePort) {
        this.integrationServicePort = integrationServicePort;
    }

    @Autowired
    public void setTableAllocationRepository(TableAllocationRepository tableAllocationRepository) {
        this.tableAllocationRepository = tableAllocationRepository;
    }

    @Autowired
    public void setReservationRepository(ReservationRepository reservationRepository) {
        this.reservationRepository = reservationRepository;
    }

    @Autowired
    public void setWalkInRepository(WalkInRepository walkInRepository) {
        this.walkInRepository = walkInRepository;
    }

    @Autowired
    public void setTableAllocationEntity2VoConverter(TableAllocationEntity2VoConverter tableAllocationEntity2VoConverter) {
        this.tableAllocationEntity2VoConverter = tableAllocationEntity2VoConverter;
    }

    @Autowired
    public void setWalkInEntity2VoConverter(WalkInEntity2VoConverter walkInEntity2VoConverter) {
        this.walkInEntity2VoConverter = walkInEntity2VoConverter;
    }

    @Autowired
    public void setReservationEntity2VoConverter(ReservationEntity2VoConverter reservationEntity2VoConverter) {
        this.reservationEntity2VoConverter = reservationEntity2VoConverter;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo22;
    private AccountVo accountVo4;

    private TableVo tableVo1;
    private TableVo tableVo2;
    private TableVo tableVo4;
    private TableVo tableVo22;

    private ReservationVo reservationVo1;
    private ReservationVo reservationVo2;
    private ReservationEntity reservationEntity1;
    private ReservationEntity reservationEntity2;

    private WalkInVo walkInVo1;
    private WalkInVo walkInVo2;
    private WalkInEntity walkInEntity1;
    private WalkInEntity walkInEntity2;

    private TableAllocationForm tableAllocationForm;
    private TableAllocationVo tableAllocationVo1;
    private TableAllocationVo tableAllocationVo2;
    private TableAllocationVo tableAllocationVo3;
    private TableAllocationVo tableAllocationVo4;
    private TableAllocationEntity tableAllocationEntity1;
    private TableAllocationEntity tableAllocationEntity2;
    private TableAllocationEntity tableAllocationEntity3;
    private TableAllocationEntity tableAllocationEntity4;

    private List<PatchOperationForm> patches;

    private TableVo tableVo(String tableId, String tableName, Boolean active, Integer capacity) {
        TableVo tableVo = new TableVo();
        tableVo.setTableId(tableId);
        tableVo.setTableName(tableName);
        tableVo.setActive(active);
        tableVo.setCapacity(capacity);
        return tableVo;
    }

    private TableAllocationEntity tableAllocationEntity(String notes, String tableId, Boolean active, Optional<? extends CheckInEntity> checkInEntity) {
        TableAllocationEntity tableAllocationEntity = new TableAllocationEntity();
        tableAllocationEntity.setNotes(notes);
        tableAllocationEntity.setTableId(tableId);
        tableAllocationEntity.setActive(active);
        tableAllocationEntity.setCheckIn(checkInEntity.get());
        return tableAllocationEntity;
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

        reservationEntity1 = this.reservationEntity(accountVo1.getId(), UUID.randomUUID().toString(), 2, "reservation 1 notes", true, LocalDate.now().plusDays(1), LocalTime.now().plusHours(1));
        reservationEntity1 = reservationRepository.save(reservationEntity1);
        reservationVo1 = this.reservationEntity2VoConverter.convert(reservationEntity1);

        reservationEntity2 = this.reservationEntity(accountVo2.getId(), UUID.randomUUID().toString(), 21, "reservation 2 notes", false, LocalDate.now().plusDays(2), LocalTime.now().plusHours(2));
        reservationEntity2 = reservationRepository.save(reservationEntity2);
        reservationVo2 = this.reservationEntity2VoConverter.convert(reservationEntity2);

        /**
         *  Walk In
         */

        walkInEntity1 = this.walkInEntity(accountVo1.getId(), UUID.randomUUID().toString(), 2, "walkIn 1 notes", true, name(), phoneNumber(), emailId());
        walkInEntity1 = walkInRepository.save(walkInEntity1);
        walkInVo1 = this.walkInEntity2VoConverter.convert(walkInEntity1);

        walkInEntity2 = this.walkInEntity(accountVo2.getId(), UUID.randomUUID().toString(), 21, "walkIn 2 notes", false, name(), phoneNumber(), emailId());
        walkInEntity2 = walkInRepository.save(walkInEntity2);
        walkInVo2 = this.walkInEntity2VoConverter.convert(walkInEntity2);

        /**
         * Table
         */

        tableVo1 = this.tableVo("1", "Table 1", true, 2);
        tableVo2 = this.tableVo("2", "Table 2", true, 2);
        tableVo4 = this.tableVo("4", "Table 4", true, 4);
        tableVo22 = this.tableVo("22", "Table 22", false, 4);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableId", "4"),
                new PatchOperationForm("replace", "/checkInId", reservationEntity1.getId().toString()));

        tableAllocationForm = new TableAllocationForm();
        tableAllocationForm.setNotes("New Something Next");
        tableAllocationForm.setCheckInId(reservationEntity1.getId().toString());
        tableAllocationForm.setTableId(tableVo4.getTableId());

        tableAllocationEntity1 = this.tableAllocationEntity("TableAllocation 1 Name", tableVo1.getTableId(), Boolean.TRUE, Optional.of(reservationEntity1));
        tableAllocationEntity1 = tableAllocationRepository.save(tableAllocationEntity1);
        tableAllocationVo1 = this.tableAllocationEntity2VoConverter.convert(tableAllocationEntity1);
        //tableAllocationVo1.setCheckIn(reservationVo1);

        tableAllocationEntity2 = this.tableAllocationEntity("TableAllocation 2 Name", tableVo2.getTableId(), Boolean.TRUE, Optional.of(reservationEntity2));
        tableAllocationEntity2 = tableAllocationRepository.save(tableAllocationEntity2);
        tableAllocationVo2 = this.tableAllocationEntity2VoConverter.convert(tableAllocationEntity2);

        tableAllocationEntity3 = this.tableAllocationEntity("TableAllocation 3 Name", tableVo4.getTableId(), Boolean.FALSE, Optional.of(walkInEntity2));
        tableAllocationEntity3 = tableAllocationRepository.save(tableAllocationEntity3);
        tableAllocationVo3 = this.tableAllocationEntity2VoConverter.convert(tableAllocationEntity3);

        tableAllocationEntity4 = this.tableAllocationEntity("TableAllocation 4 Name", tableVo22.getTableId(), Boolean.TRUE, Optional.of(walkInEntity2));
        tableAllocationEntity4 = tableAllocationRepository.save(tableAllocationEntity4);
        tableAllocationVo4 = this.tableAllocationEntity2VoConverter.convert(tableAllocationEntity4);
    }

    @AfterEach
    private void destroy() {
        tableAllocationEntity1.setCheckIn(null);
        tableAllocationEntity2.setCheckIn(null);
        tableAllocationEntity3.setCheckIn(null);
        tableAllocationEntity4.setCheckIn(null);

        tableAllocationRepository.deleteById(tableAllocationEntity1.getId());
        tableAllocationRepository.deleteById(tableAllocationEntity2.getId());
        tableAllocationRepository.deleteById(tableAllocationEntity3.getId());
        tableAllocationRepository.deleteById(tableAllocationEntity4.getId());

        reservationRepository.deleteById(reservationEntity1.getId());
        reservationRepository.deleteById(reservationEntity2.getId());
        walkInRepository.deleteById(walkInEntity1.getId());
        walkInRepository.deleteById(walkInEntity2.getId());
    }

    /**
     * POST - start
     */

    /**
     * POST with form - START
     */

    @Test
    public void test_TableAllocation_Post_ShouldReturn_201Response_And_NewTableAllocationId_WhenPosted_WithValidTableAllocationForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(TABLE_ALLOCATION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_TableAllocation_Post_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenPosted_WithNoTableAllocationForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    /**
     * POST with form - END
     */

    /**
     * POST with empty fields - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyTableId(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        tableAllocationForm.setTableId(tableId);

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithEmptyCheckInId(String checkInId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(checkInId);

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * POST with empty fields - END
     */

    /**
     * POST with invalid fields - START
     */

    @Test
    public void test_TableAllocation_Post_ShouldReturn_404Response_And_ErrorCode_RES_EAREA_001_WhenRequested_WithInvalidTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-001";
        String fieldName = "id";
        tableAllocationForm.setTableId("r");

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_TableAllocation_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInvalidCheckInId(String checkInId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(checkInId);

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * POST with invalid fields - END
     */

    /**
     * POST with absent fields - START
     */


    @Test
    public void test_TableAllocation_Post_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_002_WhenRequested_WithAbsentTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-002";
        String fieldName = "id";
        String keyword = "unavailable";
        tableAllocationForm.setTableId("3");

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_TableAllocation_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithAbsentCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(String.valueOf(Short.MAX_VALUE));

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * POST with absent fields - END
     */

    /**
     * POST with inactive fields - START
     */

    @Test
    public void test_TableAllocation_Post_ShouldReturn_500Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInactiveTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";
        String keyword = "invalid";
        tableAllocationForm.setTableId(tableVo22.getTableId());

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_TableAllocation_Post_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInactiveCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(walkInEntity2.getId().toString());

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * POST with inactive fields - END
     */

    /**
     * POST with duplicate fields - START
     */

    @Test
    public void test_TableAllocation_Post_ShouldReturn_409Response_And_ErrorCode_RES_ENGMNT_004_WhenRequested_WithDuplicateTableAllocation() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_EXISTS.getErrorCode();
        String field2Name = "checkInId";
        String field3Name = "tableId";
        String field2Value = tableAllocationEntity1.getCheckIn().getId().toString();
        String field3Value = tableAllocationEntity1.getTableId();
        tableAllocationForm.setCheckInId(field2Value);
        tableAllocationForm.setTableId(field3Value);

        mvcResult = mockMvc.perform(post(TABLE_ALLOCATION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Name));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Value));
    }

    /**
     * POST with duplicate fields - END
     */

    /**
     * POST - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * PUT - start
     */

    /**
     * PUT with form - START
     */

    @Test
    public void test_TableAllocation_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTableAllocationDetails() throws Exception {
        String id = tableAllocationEntity1.getId().toString();
        MvcResult mvcResult = null;
        tableAllocationForm.setNotes("Updated notes");
        tableAllocationForm.setTableId(tableVo2.getTableId());

        mvcResult = this.mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TableAllocation_Put_ShouldReturn_200Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdatedById_WithNoTableAllocationForm() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TableAllocation_Put_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdatedById_WithNoTableAllocationForm() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    /**
     * PUT with form - END
     */

    /**
     * PUT with empty fields - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithEmptyTableId(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        tableAllocationForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }


    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithEmptyNotes(String instructions) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "notes";
        tableAllocationForm.setNotes(instructions);

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithEmptyCheckInId(String checkInId) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(checkInId);

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PUT with empty fields - END
     */

    /**
     * PUT with invalid fields - START
     */

    @Test
    public void test_TableAllocation_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenUpdatedById_WithInvalidTableId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = "RES-EAREA-001";
        String fieldName = "id";
        tableAllocationForm.setTableId("r");

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_TableAllocation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithInvalidCheckInId(String checkInId) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(checkInId);

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PUT with invalid fields - END
     */

    /**
     * PUT with absent fields - START
     */

    @Test
    public void test_TableAllocation_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_002_WhenUpdatedById_WithAbsentTableId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = "RES-EAREA-002";
        String fieldName = "id";
        String keyword = "unavailable";
        tableAllocationForm.setTableId("3");

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_TableAllocation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithAbsentCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(String.valueOf(Short.MAX_VALUE));

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PUT with absent fields - END
     */

    /**
     * PUT with inactive fields - START
     */

    @Test
    public void test_TableAllocation_Put_ShouldReturn_500Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInactiveTableId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";
        String keyword = "invalid";
        tableAllocationForm.setTableId(tableVo22.getTableId());

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_TableAllocation_Put_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_WithInactiveCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        tableAllocationForm.setCheckInId(walkInEntity2.getId().toString());

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PUT with inactive fields - END
     */

    /**
     * PUT with duplicate fields - START
     */

    @Test
    public void test_TableAllocation_Put_ShouldReturn_409Response_And_ErrorCode_RES_ENGMNT_004_WhenUpdatedById_WithDuplicateTableAllocation() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity2.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_EXISTS.getErrorCode();
        String field2Name = "checkInId";
        String field3Name = "tableId";
        String field2Value = tableAllocationEntity1.getCheckIn().getId().toString();
        String field3Value = tableAllocationEntity1.getTableId();
        tableAllocationForm.setCheckInId(field2Value);
        tableAllocationForm.setTableId(field3Value);

        mvcResult = mockMvc.perform(put(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(tableAllocationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Name));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Value));
    }

    /**
     * PUT with duplicate fields - END
     */

    /**
     * PUT - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * PATCH - start
     */

    /**
     * PATCH with form - START
     */

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdatedById_WithValidTableAllocationAttributes() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();

        mvcResult = this.mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.isEmpty(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_422Response_And_ErrorCode_RES_ENGMNT_003_WhenUpdatedById_WithNoTableAllocationAttributes() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /**
     * PATCH with form - END
     */

    /**
     * PATCH with empty fields - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyTableId(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableId", tableId));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }


    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyNotes(String notes) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/notes", notes));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyActive(String active) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", active));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyCheckInId(String checkInId) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/checkInId", checkInId));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    /**
     * Empty checks against fields of primitive types not possible
     */

    /**
     * PATCH with empty fields - END
     */

    /**
     * PATCH with invalid fields - START
     */

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ById_AndInvalidDefinitionOfTableAllocationAttribute() throws Exception {
        String id = tableAllocationEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenUpdatedById_WithInvalidTableId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = "RES-EAREA-001";
        String keyword = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableId", "r"));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithInvalidCheckInId(String checkInId) throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, checkInId));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PATCH with invalid fields - END
     */

    /**
     * PATCH with absent fields - START
     */

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithAbsentTableId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        String keyword = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "3"));


        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithAbsentCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, String.valueOf(Short.MAX_VALUE)));


        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PATCH with absent fields - END
     */

    /**
     * PATCH with inactive fields - START
     */

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithInactiveTableId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        String keyword = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, tableVo22.getTableId()));


        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenUpdatedById_WithInactiveCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity1.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";
        String keyword = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, walkInEntity2.getId().toString()));


        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    /**
     * PATCH with inactive fields - END
     */

    /**
     * PATCH with duplicate fields - START
     */

    @Test
    public void test_TableAllocation_Patch_ShouldReturn_409Response_And_ErrorCode_RES_ENGMNT_004_WhenUpdatedById_WithDuplicateTableAllocation() throws Exception {
        MvcResult mvcResult = null;
        String id = tableAllocationEntity2.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_EXISTS.getErrorCode();
        String field2Name = "checkInId";
        String field3Name = "tableId";
        String field2Value = tableAllocationEntity1.getCheckIn().getId().toString();
        String field3Value = tableAllocationEntity1.getTableId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field2Name, field2Value),
                new PatchOperationForm("replace", "/" + field3Name, field3Value));

        mvcResult = mockMvc.perform(patch(TABLE_ALLOCATION_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(objectMapper.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Name));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Value));
    }

    /**
     * PATCH with duplicate fields - END
     */

    /**
     * PATCH - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * DELETE - START
     */

    @Test
    public void test_TableAllocation_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = tableAllocationEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(TABLE_ALLOCATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.isEmpty(mvcResult.getResponse().getContentAsString()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_TableAllocation_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001__WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TABLE_ALLOCATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TableAllocation_Delete_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = tableAllocationEntity3.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(TABLE_ALLOCATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TableAllocation_Delete_ShouldReturn_404Response_And_ErrorCode_RES_ENGMNT_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TABLE_ALLOCATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /**
     * DELETE - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * GET - START
     */

    /**
     * GET all - START
     */

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationListNaturallyOrdered_WhenRequested_ForAllTableAllocations() throws Exception {
        MvcResult mvcResult = null;
        List<TableAllocationVo> tableAllocationList = new ArrayList<>(Arrays.asList(tableAllocationVo1, tableAllocationVo2, tableAllocationVo3, tableAllocationVo4));

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
    }

    /**
     * GET all - END
     */

    /**
     * GET all by path variable - START
     */

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationDetails_WhenRequested_ById() throws Exception {
        String id = tableAllocationEntity1.getId().toString();
        MvcResult mvcResult = null;
        tableAllocationVo1.setCheckIn(null);

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(objectMapper.writeValueAsString(tableAllocationVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(tableAllocationVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_TableAllocation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /**
     * Get by inactive primary ID not possible because of first and second level cascades
     */

    @Test
    public void test_TableAllocation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = tableAllocationEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getId());
        Assertions.assertEquals(tableAllocationVo1.getNotes(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getNotes());
        Assertions.assertEquals(tableAllocationVo1.getTableId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getTableId());
        Assertions.assertEquals(tableAllocationVo1.getCheckInId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getCheckInId());
        Assertions.assertTrue(ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getActive()));
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = tableAllocationEntity1.getId().toString();
        MvcResult mvcResult = null;
        tableAllocationVo1.setTable(tableVo1);
        tableAllocationVo1.setCheckIn(reservationVo1);

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationVo1.getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getId());
        Assertions.assertEquals(tableAllocationVo1.getNotes(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getNotes());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getTable() != null);
        Assertions.assertEquals(tableAllocationVo1.getTable().getTableId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getTable().getTableId());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getCheckIn() != null);
        Assertions.assertEquals(tableAllocationVo1.getCheckIn().getId(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getCheckIn().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo.class).getActive()));
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationListNaturallyOrdered_WhenRequested_ForTableAllocations_ByCheckInId() throws Exception {
        MvcResult mvcResult = null;

        List<TableAllocationVo> tableAllocationList = Arrays.asList(tableAllocationVo1);
        tableAllocationVo1.setCheckIn(null);

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_CHECKIN_ID, reservationEntity1.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
        Assertions.assertEquals(objectMapper.writeValueAsString(tableAllocationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationListNaturallyOrdered_WhenRequested_ForTableAllocations_ByEmptyCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String checkInId = " ";
        String fieldName = "checkInId";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_CHECKIN_ID, checkInId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequested_ByInvalidCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String checkInId = "kk";
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "checkInId";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_CHECKIN_ID, checkInId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_500Response_And_ErrorCode_RES_ENGMNT_002_WhenRequested_ByAbsentCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String checkInId = String.valueOf(Long.MAX_VALUE);
        String errorCode = EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE.getErrorCode();
        String fieldName = "checkInId";
        String message1 = "search";
        String message2 = "not found";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_CHECKIN_ID, checkInId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message1));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message2));
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_500Response_And_ErrorCode_RES_ENGMNT_006_WhenRequested_ByInactiveCheckInId() throws Exception {
        MvcResult mvcResult = null;
        String checkInId = reservationEntity2.getId().toString();
        String errorCode = EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE.getErrorCode();
        String fieldName = "checkInId";
        String message1 = "search";
        String message2 = "deactivated";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_BY_CHECKIN_ID, checkInId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message1));
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message2));
    }

    /**
     * GET all by path variable - END
     */

    /**
     * GET all by empty filters - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyNotesOnly(String notes) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER).queryParam("notes", notes))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyActiveOnly(String active) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER).queryParam("active", active))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_TableAllocation_Get_ShouldReturn_400Response_And_ErrorCode_RES_ENGMNT_001_WhenRequestedBy_EmptyTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /**
     * GET all by empty filters - END
     */

    /**
     * GET all by invalid filters - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "r", "999999999999999" })
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_EmptyTableAllocationList_WhenRequestedBy_InvalidTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
    }

    /**
     * Invalid notes not possible as it is free range text
     * Invalid active not possible since it would always resolve to FALSE because of active attribute's type
     */

    /**
     * GET all by invalid filters - END
     */

    /**
     * GET all by absent filters - START
     */

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_EmptyTableAllocationList_WhenRequestedBy_AbsentNotesOnly() throws Exception {
        MvcResult mvcResult = null;
        String notes = "x";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER).queryParam("notes", notes))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_EmptyTableAllocationList_WhenRequestedBy_AbsentActiveOnly() throws Exception {
        MvcResult mvcResult = null;
        tableAllocationVo2.setCheckIn(null);
        List<TableAllocationVo> tableAllocationList = new ArrayList<>(Arrays.asList(tableAllocationVo3));

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("active", "x"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
        Assertions.assertEquals(objectMapper.writeValueAsString(tableAllocationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_EmptyTableAllocationList_WhenRequestedBy_AbsentTableIdOnly() throws Exception {
        MvcResult mvcResult = null;
        String tableId = "x";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_EmptyTableAllocationList_WhenRequestedBy_TableIdAndAbsentNotes() throws Exception {
        MvcResult mvcResult = null;
        String tableId = "x";

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("tableId", tableId)
                        .queryParam("notes", "absent"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
    }

    /**
     * GET all by absent filters - END
     */

    /**
     * GET all by filter combination - START
     */

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationListNaturallyOrdered_WhenRequested_ForTableAllocations_WithNotes() throws Exception {
        MvcResult mvcResult = null;
        List<TableAllocationVo> tableAllocationList = new ArrayList<>(Arrays.asList(tableAllocationVo1));
        tableAllocationVo1.setCheckIn(null);

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("notes", "TableAllocation 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
        Assertions.assertEquals(objectMapper.writeValueAsString(tableAllocationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationListNaturallyOrdered_WhenRequested_ForTableAllocations_WithActive() throws Exception {
        MvcResult mvcResult = null;
        tableAllocationVo2.setCheckIn(null);
        List<TableAllocationVo> tableAllocationList = new ArrayList<>(Arrays.asList(tableAllocationVo1, tableAllocationVo2, tableAllocationVo4));

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("active", "true"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
        Assertions.assertEquals(objectMapper.writeValueAsString(tableAllocationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationListNaturallyOrdered_WhenRequested_ForTableAllocations_WithTableId() throws Exception {
        MvcResult mvcResult = null;
        tableAllocationVo1.setCheckIn(null);
        tableAllocationVo2.setCheckIn(null);
        tableAllocationVo3.setCheckIn(null);
        List<TableAllocationVo> tableAllocationList = new ArrayList<>(Arrays.asList(tableAllocationVo1));
        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("tableId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
        Assertions.assertEquals(objectMapper.writeValueAsString(tableAllocationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_TableAllocationListNaturallyOrdered_WhenRequested_ForTableAllocations_WithTableIdAndNotes() throws Exception {
        MvcResult mvcResult = null;
        List<TableAllocationVo> tableAllocationList = Arrays.asList(tableAllocationVo1);
        tableAllocationVo1.setCheckIn(null);

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("notes", "TableAllocation")
                        .queryParam("tableId", tableAllocationEntity1.getTableId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
        Assertions.assertEquals(objectMapper.writeValueAsString(tableAllocationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_EmptyTableAllocationList_WhenRequested_ForTableAllocations_WithAbsent_NotesAndActive() throws Exception {
        MvcResult mvcResult = null;
        List<TableAllocationVo> tableAllocationList = Arrays.asList(tableAllocationVo1);
        tableAllocationVo1.setCheckIn(null);

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("notes", "TableAllocation 1")
                        .queryParam("active", "true"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
    }

    @Test
    public void test_TableAllocation_Get_ShouldReturn_200Response_And_EmptyTableAllocationList_WhenRequested_ForTableAllocations_WithAbsent_NotesAndTableIdAndActive() throws Exception {
        MvcResult mvcResult = null;
        List<TableAllocationVo> tableAllocationList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(TABLE_ALLOCATION_URI_FILTER)
                        .queryParam("notes", "TableAllocation 1")
                        .queryParam("tableId", UUID.randomUUID().toString())
                        .queryParam("active", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableAllocationList.size(), objectMapper.readValue(mvcResult.getResponse().getContentAsString(), TableAllocationVo[].class).length);
    }

    /**
     * GET all by filter combination - END
     */

    @Override
    public String getSimulationBaseLocation() {
        return "integration";
    }

    @Override
    public Integer getServicePort() {
        return this.integrationServicePort;
    }

    @Override
    public String[] getSimulationFilePaths() {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation.json") };
    }
}
