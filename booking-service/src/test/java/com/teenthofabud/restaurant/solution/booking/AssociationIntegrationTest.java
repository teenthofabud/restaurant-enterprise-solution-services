package com.teenthofabud.restaurant.solution.booking;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceDocument;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceVo;
import com.teenthofabud.restaurant.solution.booking.experience.repository.ExperienceRepository;
import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDocument;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationForm;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.booking.association.repository.AssociationRepository;
import com.teenthofabud.restaurant.solution.booking.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.booking.integration.establishmentarea.data.TableVo;
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
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
public class AssociationIntegrationTest extends BookingIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ITEM_URI = "/association";
    private static final String ITEM_URI_BY_ID = "/association/{id}";
    private static final String ITEM_URI_BY_EXPERIENCE_ID = "/association/experienceid/{experienceId}";
    private static final String ITEM_URI_FILTER = "/association/filter";

    private AssociationRepository associationRepository;
    private ExperienceRepository experienceRepository;

    private Integer bookingIntegrationPort;

    @Value("${booking.integration.port}")
    public void setBookingIntegrationPort(Integer bookingIntegrationPort) {
        this.bookingIntegrationPort = bookingIntegrationPort;
    }

    @Autowired
    public void setAssociationRepository(AssociationRepository associationRepository) {
        this.associationRepository = associationRepository;
    }

    @Autowired
    public void setExperienceRepository(ExperienceRepository experienceRepository) {
        this.experienceRepository = experienceRepository;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo4;
    private AccountVo accountVo22;

    private TableVo tableVo1;
    private TableVo tableVo2;
    private TableVo tableVo4;
    private TableVo tableVo22;

    private ExperienceVo experienceVo1;
    private ExperienceVo experienceVo2;
    private ExperienceVo experienceVo3;
    private ExperienceVo experienceVo4;
    private ExperienceDocument experienceDocument1;
    private ExperienceDocument experienceDocument2;
    private ExperienceDocument experienceDocument3;
    private ExperienceDocument experienceDocument4;

    private AssociationForm associationForm;
    private AssociationVo associationVo1;
    private AssociationVo associationVo2;
    private AssociationVo associationVo3;
    private AssociationVo associationVo4;
    private AssociationVo associationVo5;
    private AssociationVo associationVo6;
    private AssociationDocument associationDocument1;
    private AssociationDocument associationDocument2;
    private AssociationDocument associationDocument3;
    private AssociationDocument associationDocument4;
    private AssociationDocument associationDocument5;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Experience
         */

        experienceDocument1 = new ExperienceDocument();
        experienceDocument1.setName("Experience 1 Name");
        experienceDocument1.setDescription("Experience 1 TableId");
        experienceDocument1.setActive(Boolean.TRUE);

        experienceDocument1 = experienceRepository.save(experienceDocument1);

        experienceVo1 = new ExperienceVo();
        experienceVo1.setId(experienceDocument1.getId());
        experienceVo1.setName(experienceDocument1.getName());
        experienceVo1.setDescription(experienceDocument1.getDescription());

        experienceDocument2 = new ExperienceDocument();
        experienceDocument2.setName("Experience 2 Name");
        experienceDocument2.setDescription("Experience 2 TableId");
        experienceDocument2.setActive(Boolean.FALSE);

        experienceDocument2 = experienceRepository.save(experienceDocument2);

        experienceVo2 = new ExperienceVo();
        experienceVo2.setId(experienceDocument2.getId().toString());
        experienceVo2.setName(experienceDocument2.getName());
        experienceVo2.setDescription(experienceDocument2.getDescription());

        experienceDocument3 = new ExperienceDocument();
        experienceDocument3.setName("Experience 3 Name");
        experienceDocument3.setDescription("Experience 3 TableId");
        experienceDocument3.setActive(Boolean.TRUE);

        experienceDocument3 = experienceRepository.save(experienceDocument3);

        experienceVo3 = new ExperienceVo();
        experienceVo3.setId(experienceDocument3.getId().toString());
        experienceVo3.setName(experienceDocument3.getName());
        experienceVo3.setDescription(experienceDocument3.getDescription());

        experienceDocument4 = new ExperienceDocument();
        experienceDocument4.setName("Experience 4 Name");
        experienceDocument4.setDescription("Experience 4 TableId");
        experienceDocument4.setActive(Boolean.TRUE);

        experienceDocument4 = experienceRepository.save(experienceDocument4);

        experienceVo4 = new ExperienceVo();
        experienceVo4.setId(experienceDocument4.getId());
        experienceVo4.setName(experienceDocument4.getName());
        experienceVo4.setDescription(experienceDocument4.getDescription());

        /**
         * Account
         */

        accountVo1 = new AccountVo();
        accountVo1.setFirstName("Account 1");
        accountVo1.setLastName("Account 1");
        accountVo1.setActive(Boolean.TRUE);
        accountVo1.setId("1");

        accountVo2 = new AccountVo();
        accountVo2.setFirstName("Account 2");
        accountVo2.setLastName("Account 2");
        accountVo2.setActive(Boolean.TRUE);
        accountVo2.setId("2");

        accountVo4 = new AccountVo();
        accountVo4.setFirstName("Account 4");
        accountVo4.setLastName("Account 4");
        accountVo4.setActive(Boolean.TRUE);
        accountVo4.setId("4");

        accountVo22 = new AccountVo();
        accountVo22.setFirstName("Account 22");
        accountVo22.setLastName("Account 22");
        accountVo22.setActive(Boolean.FALSE);
        accountVo22.setId("22");

        /**
         * Table
         */

        tableVo1 = new TableVo();
        tableVo1.setTableId("1");
        tableVo1.setTableName("Table 1");
        tableVo1.setActive(Boolean.TRUE);

        tableVo2 = new TableVo();
        tableVo2.setTableId("2");
        tableVo2.setTableName("Table 2");
        tableVo2.setActive(Boolean.TRUE);

        tableVo4 = new TableVo();
        tableVo4.setTableId("4");
        tableVo4.setTableName("Table 4");
        tableVo4.setActive(Boolean.TRUE);

        tableVo22 = new TableVo();
        tableVo22.setTableId("22");
        tableVo22.setTableName("Table 22");
        tableVo22.setActive(Boolean.FALSE);

        /**
         * Association
         */

        associationForm = new AssociationForm();
        associationForm.setTableId("4");
        associationForm.setExperienceId(experienceDocument3.getId());
        associationForm.setAccountId("4");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"),
                new PatchOperationForm("replace", "/tableId", "4"));

        associationDocument1 = new AssociationDocument();
        associationDocument1.setAccountId("1");
        associationDocument1.setTableId("1");
        associationDocument1.setActive(Boolean.TRUE);
        associationDocument1.setExperienceId(experienceDocument1.getId());

        associationDocument1 = associationRepository.save(associationDocument1);

        associationVo1 = new AssociationVo();
        associationVo1.setId(associationDocument1.getId());
        associationVo1.setExperienceId(experienceDocument1.getId());
        associationVo1.setTableId(associationDocument1.getTableId());
        associationVo1.setAccountId(associationDocument1.getAccountId());
        //associationVo1.setExperience(experienceVo1);

        associationDocument2 = new AssociationDocument();
        associationDocument2.setAccountId("1");
        associationDocument2.setTableId("2");
        associationDocument2.setActive(Boolean.FALSE);
        associationDocument2.setExperienceId(experienceDocument2.getId());

        associationDocument2 = associationRepository.save(associationDocument2);

        associationVo2 = new AssociationVo();
        associationVo2.setId(associationDocument2.getId());
        associationVo2.setExperienceId(experienceDocument2.getId());
        associationVo2.setTableId(associationDocument2.getTableId());
        associationVo2.setAccountId(associationDocument2.getAccountId());
        //associationVo2.setExperience(experienceVo2);

        associationDocument3 = new AssociationDocument();
        associationDocument3.setAccountId("2");
        associationDocument3.setTableId("1");
        associationDocument3.setActive(Boolean.TRUE);
        associationDocument3.setExperienceId(experienceDocument3.getId());

        associationDocument3 = associationRepository.save(associationDocument3);

        associationVo3 = new AssociationVo();
        associationVo3.setId(associationDocument3.getId());
        associationVo3.setExperienceId(experienceDocument3.getId());
        associationVo3.setTableId(associationDocument3.getTableId());
        associationVo3.setAccountId(associationDocument3.getAccountId());
        //associationVo3.setExperience(experienceVo3);

        associationDocument4 = new AssociationDocument();
        associationDocument4.setAccountId("2");
        associationDocument4.setTableId("4");
        associationDocument4.setActive(Boolean.TRUE);
        associationDocument4.setExperienceId(experienceDocument4.getId());

        associationDocument4 = associationRepository.save(associationDocument4);

        associationVo4 = new AssociationVo();
        associationVo4.setId(associationDocument4.getId());
        associationVo4.setExperienceId(experienceDocument4.getId());
        associationVo4.setTableId(associationDocument4.getTableId());
        associationVo4.setAccountId(associationDocument4.getAccountId());
        //associationVo4.setExperience(experienceVo4);

        associationDocument5 = new AssociationDocument();
        associationDocument5.setAccountId("4");
        associationDocument5.setTableId("2");
        associationDocument5.setActive(Boolean.TRUE);
        associationDocument5.setExperienceId(experienceDocument4.getId());

        associationDocument5 = associationRepository.save(associationDocument5);

        associationVo5 = new AssociationVo();
        associationVo5.setId(associationDocument5.getId());
        associationVo5.setExperienceId(experienceDocument4.getId());
        associationVo5.setTableId(associationDocument5.getTableId());
        associationVo5.setAccountId(associationDocument5.getAccountId());
        //associationVo5.setExperience(experienceVo4);

        associationVo6 = new AssociationVo();
        associationVo6.setId(UUID.randomUUID().toString());
        associationVo6.setExperienceId(associationForm.getExperienceId());
        associationVo6.setTableId(associationForm.getTableId());
        associationVo6.setAccountId(associationForm.getAccountId());
        //associationVo6.setExperience(experienceVo3);

    }

    @AfterEach
    private void destroy() {
        associationRepository.deleteById(associationDocument1.getId());
        associationRepository.deleteById(associationDocument2.getId());
        associationRepository.deleteById(associationDocument3.getId());
        associationRepository.deleteById(associationDocument4.getId());
        associationRepository.deleteById(associationDocument5.getId());

        experienceRepository.deleteById(experienceDocument1.getId());
        experienceRepository.deleteById(experienceDocument2.getId());
        experienceRepository.deleteById(experienceDocument3.getId());
        experienceRepository.deleteById(experienceDocument4.getId());
    }

    @Test
    public void test_Association_Post_ShouldReturn_201Response_And_NewAssociationId_WhenPosted_WithValidAssociationForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }
    
    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithEmptyAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "accountId";
        associationForm.setAccountId("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithEmptyExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        associationForm.setExperienceId("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithEmptyTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        associationForm.setTableId("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInactiveAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "accountId";
        associationForm.setAccountId("22");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInactiveExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        associationForm.setExperienceId(experienceDocument2.getId().toString());

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInactiveTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        associationForm.setTableId("22");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithInvalidAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String fieldName = "invalid";
        associationForm.setAccountId("r");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInvalidExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        associationForm.setExperienceId("r");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_WithInvalidTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-001";
        String fieldName = "invalid";
        associationForm.setTableId("r");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_002_WhenRequested_WithAbsentAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-002";
        String fieldName = "unavailable";
        associationForm.setAccountId("3");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithAbsentExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        associationForm.setExperienceId("99999");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_500Response_And_ErrorCode_RES_EREA_002_WhenRequested_WithAbsentTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-002";
        String fieldName = "unavailable";
        associationForm.setTableId("3");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Post_ShouldReturn_409Response_And_ErrorCode_RES_BOOKING_004_WhenRequested_WithDuplicateAssociation() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "tableId";
        String field1Value = associationDocument1.getAccountId();
        String field2Value = associationDocument1.getTableId();
        associationForm.setAccountId(field1Value);
        associationForm.setTableId(field2Value);

        mvcResult = mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
    }

    @Test
    public void test_Association_Post_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenPosted_WithNoAssociationForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_AssociationListNaturallyOrdered_WhenRequested_ForAllAssociations() throws Exception {
        MvcResult mvcResult = null;
        List<AssociationVo> associationList = new ArrayList<>(Arrays.asList(associationVo6, associationVo1, associationVo2, associationVo3, associationVo5, associationVo4));

        mvcResult = this.mockMvc.perform(get(ITEM_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(associationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_AssociationListNaturallyOrdered_WhenRequested_ForAssociations_ByExperienceId() throws Exception {
        MvcResult mvcResult = null;
        associationVo4.setExperience(null);
        associationVo5.setExperience(null);
        List<AssociationVo> associationList = Arrays.asList(associationVo4, associationVo5);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_EXPERIENCE_ID, experienceDocument4.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(associationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(associationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_AssociationListNaturallyOrdered_WhenRequested_ForAssociations_ByEmptyExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_EXPERIENCE_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Get_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ByAbsentExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String experienceId = "kk";
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_EXPERIENCE_ID, experienceId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(experienceId));
    }

    @Test
    public void test_Association_Get_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ByInactiveExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String experienceId = experienceDocument2.getId();
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String errorName = "invalid";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_EXPERIENCE_ID, experienceId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(errorName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(experienceId));
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_AssociationListNaturallyOrdered_WhenRequested_ForAssociations_WithAccountId() throws Exception {
        MvcResult mvcResult = null;
        List<AssociationVo> associationList = new ArrayList<>(Arrays.asList(associationVo1, associationVo2));

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("accountId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(associationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(associationList), mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Association_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "22" })
    public void test_Association_Get_ShouldReturn_200Response_And_EmptyAssociationList_WhenRequestedBy_InactiveAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Association_Get_ShouldReturn_200Response_And_EmptyAssociationList_WhenRequestedBy_AbsentAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_AssociationListNaturallyOrdered_WhenRequested_ForAssociations_WithTableId() throws Exception {
        MvcResult mvcResult = null;
        associationVo1.setExperience(null);
        List<AssociationVo> associationList = new ArrayList<>(Arrays.asList(associationVo1, associationVo3));

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("tableId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(associationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(associationList), mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Association_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "22" })
    public void test_Association_Get_ShouldReturn_200Response_And_EmptyAssociationList_WhenRequestedBy_InactiveTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Association_Get_ShouldReturn_200Response_And_EmptyAssociationList_WhenRequestedBy_AbsentTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
    }

    @Test
    public void test_Association_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001__WhenRequestedBy_UnsupportedFilterAttribute() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("experienceId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_AssociationListNaturallyOrdered_WhenRequested_ForAssociations_WithAccountIdAndTableId() throws Exception {
        MvcResult mvcResult = null;
        associationVo3.setExperience(null);
        associationVo2.setExperience(null);
        associationVo4.setExperience(null);
        associationVo1.setExperience(null);
        associationVo5.setExperience(null);
        List<AssociationVo> associationList = Arrays.asList(associationVo1);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("accountId", "1")
                        .queryParam("tableId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(associationList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(associationList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_AssociationDetails_WhenRequested_ById() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        associationVo1.setExperience(null);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(associationVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(associationVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Association_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = associationDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(associationVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getId());
        Assertions.assertEquals(associationVo1.getTableId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getTableId());
        Assertions.assertEquals(associationVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getAccountId());
        Assertions.assertEquals(associationVo1.getExperienceId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getExperienceId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getActive()));
    }

    @Test
    public void test_Association_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = associationDocument1.getId();
        associationVo1.setAccount(accountVo1);
        associationVo1.setTable(tableVo1);
        associationVo1.setExperience(experienceVo1);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(associationVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getTable() != null);
        Assertions.assertEquals(associationVo1.getTable().getTableId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getTable().getTableId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getAccount() != null);
        Assertions.assertEquals(associationVo1.getAccount().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getAccount().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getExperience() != null);
        Assertions.assertEquals(associationVo1.getExperience().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getExperience().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AssociationVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Association_Delete_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenDeleted_ByEmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Delete_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = associationDocument2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Association_Delete_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndAssociationDetails() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        associationForm.setAccountId("1");
        associationForm.setTableId("4");

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenUpdatedBy_EmptyId_AndAssociationDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Put_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ByAbsentId_AndAssociationDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_005_WhenUpdated_ByInactiveId_AndAssociationDetails() throws Exception {
        String id = associationDocument2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Association_Put_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndNoAssociationDetails() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /*@ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyName(String name) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        associationForm.setName(name);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyAccountId(String accountId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";
        associationForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Association_Put_ShouldReturn_500Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidAccountId(String accountId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String fieldName = "id";
        String message = "invalid";
        associationForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyInvalidExperienceId(String experienceId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        associationForm.setExperienceId(experienceId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyTableId(String tableId) throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        associationForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Association_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_ById_AndInvalidTableId(String tableId) throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-001";
        String fieldName = "id";
        String message = "invalid";
        associationForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Association_Put_ShouldReturn_500Response_And_ErrorCode_RES_CUST_002_WhenRequested_ById_AndAbsentAccountId(String accountId) throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-002";
        String fieldName = "id";
        associationForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(accountId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999999" })
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndAbsentExperienceId(String experienceId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        associationForm.setExperienceId(experienceId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Association_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_002_WhenRequested_ById_AndAbsentTableId(String tableId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-002";
        String fieldName = "id";
        associationForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(tableId));
    }

    @Test
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInactiveAccountId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String accountId = "22";
        String fieldName = "accountId";
        associationForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInactiveExperienceId() throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String experienceId = experienceDocument2.getId();
        String fieldName = "experienceId";
        associationForm.setExperienceId(experienceId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInactiveTableId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String tableId = "22";
        String fieldName = "tableId";
        associationForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Put_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndEmptyAssociationDetails() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new AssociationForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Association_Put_ShouldReturn_409Response_And_ErrorCode_RES_BOOKING_004_WhenUpdated_ById_AndDuplicateAssociationDetails() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "tableId";
        String field1Value = associationDocument1.getAccountId();
        String field2Value = associationDocument1.getTableId();
        associationForm.setAccountId(field1Value);
        associationForm.setTableId(field2Value);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(associationForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
    }

    @Test
    public void test_Association_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndAssociationDetails() throws Exception {
        String id = associationDocument1.getId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "4"),
                new PatchOperationForm("replace", "/tableId", "1"));
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndAssociationDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ITEM_URI_BY_ID, " ")
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Association_Patch_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ByAbsentId_AndAssociationDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Association_Patch_ShouldReturn_409Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ById_AndDuplicateAssociationDetails() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "tableId";
        String field1Value = associationDocument1.getAccountId();
        String field2Value = associationDocument1.getTableId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value));


        mvcResult = this.mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
    }

    @Test
    public void test_Association_Patch_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndNoAssociationDetails() throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAccountId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", " "));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyExperienceId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/experienceId", " "));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyTableId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableId", " "));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAccountId(String accountId) throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", accountId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyExperienceId(String experienceId) throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/experienceId", experienceId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyTableId(String tableId) throws Exception {
        String id = associationDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableId", tableId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidAccountId(String accountId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "accountId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, accountId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidExperienceId(String experienceId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, experienceId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidTableId(String tableId) throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, tableId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveAccountId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String accountId = "22";
        String fieldName = "accountId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, accountId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveExperienceId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String experienceId = experienceDocument2.getId().toString();
        String fieldName = "experienceId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, experienceId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveTableId() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String tableId = "22";
        String fieldName = "tableId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, tableId));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Association_Patch_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidDefinitionOfAssociationAttribute() throws Exception {
        String id = associationDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ITEM_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Override
    public String getSimulationBaseLocation() throws UnsupportedOperationException {
        return "simulation";
    }

    @Override
    public Integer getServicePort() throws UnsupportedOperationException {
        return this.bookingIntegrationPort;
    }

    @Override
    public String[] getSimulationFilePaths() throws UnsupportedOperationException {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation.json") };
    }
}
