package com.teenthofabud.restaurant.solution.settings;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodDocument;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodForm;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodVo;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.repository.PaymentMethodRepository;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
public class PaymentMethodIntegrationTest extends SettingsIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String CATEGORY_URI = "/paymentmethod";
    private static final String CATEGORY_URI_BY_ID = "/paymentmethod/{id}";
    private static final String CATEGORY_URI_FILTER = "/paymentmethod/filter";

    private PaymentMethodRepository paymentMethodRepository;

    @Autowired
    public void setPaymentMethodRepository(PaymentMethodRepository paymentMethodRepository) {
        this.paymentMethodRepository = paymentMethodRepository;
    }
    
    private PaymentMethodForm paymentMethodForm;
    private PaymentMethodVo paymentMethodVo1;
    private PaymentMethodVo paymentMethodVo2;
    private PaymentMethodVo paymentMethodVo3;
    private PaymentMethodVo paymentMethodVo4;
    private PaymentMethodVo paymentMethodVo5;
    private PaymentMethodDocument paymentMethodDocument1;
    private PaymentMethodDocument paymentMethodDocument2;
    private PaymentMethodDocument paymentMethodDocument3;
    private PaymentMethodDocument paymentMethodDocument4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * PaymentMethod
         */

        paymentMethodForm = new PaymentMethodForm();
        paymentMethodForm.setName("New Name");
        paymentMethodForm.setDescription("New Description");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"));

        paymentMethodDocument1 = new PaymentMethodDocument();
        paymentMethodDocument1.setName("PaymentMethod 1 Name");
        paymentMethodDocument1.setDescription("PaymentMethod 1 Description");
        paymentMethodDocument1.setActive(Boolean.TRUE);

        paymentMethodDocument2 = new PaymentMethodDocument();
        paymentMethodDocument2.setName("PaymentMethod 2 Name");
        paymentMethodDocument2.setDescription("PaymentMethod 2 Description");
        paymentMethodDocument2.setActive(Boolean.TRUE);

        paymentMethodDocument3 = new PaymentMethodDocument();
        paymentMethodDocument3.setName("PaymentMethod 3 Name");
        paymentMethodDocument3.setDescription("PaymentMethod 3 Description");
        paymentMethodDocument3.setActive(Boolean.TRUE);

        paymentMethodDocument4 = new PaymentMethodDocument();
        paymentMethodDocument4.setName("PaymentMethod 4 Name");
        paymentMethodDocument4.setDescription("PaymentMethod 4 Description");
        paymentMethodDocument4.setActive(Boolean.FALSE);

        paymentMethodDocument1 = paymentMethodRepository.save(paymentMethodDocument1);

        paymentMethodVo1 = new PaymentMethodVo();
        paymentMethodVo1.setId(paymentMethodDocument1.getId().toString());
        paymentMethodVo1.setName(paymentMethodDocument1.getName());
        paymentMethodVo1.setDescription(paymentMethodDocument1.getDescription());

        paymentMethodDocument2 = paymentMethodRepository.save(paymentMethodDocument2);

        paymentMethodVo2 = new PaymentMethodVo();
        paymentMethodVo2.setId(paymentMethodDocument2.getId().toString());
        paymentMethodVo2.setName(paymentMethodDocument2.getName());
        paymentMethodVo2.setDescription(paymentMethodDocument2.getDescription());

        paymentMethodDocument3 = paymentMethodRepository.save(paymentMethodDocument3);

        paymentMethodVo3 = new PaymentMethodVo();
        paymentMethodVo3.setId(paymentMethodDocument3.getId().toString());
        paymentMethodVo3.setName(paymentMethodDocument3.getName());
        paymentMethodVo3.setDescription(paymentMethodDocument3.getDescription());

        paymentMethodDocument4 = paymentMethodRepository.save(paymentMethodDocument4);

        paymentMethodVo4 = new PaymentMethodVo();
        paymentMethodVo4.setId(paymentMethodDocument4.getId().toString());
        paymentMethodVo4.setName(paymentMethodDocument4.getName());
        paymentMethodVo4.setDescription(paymentMethodDocument4.getDescription());

        paymentMethodVo5 = new PaymentMethodVo();
        paymentMethodVo5.setId(UUID.randomUUID().toString());
        paymentMethodVo5.setName(paymentMethodForm.getName());
        paymentMethodVo5.setDescription(paymentMethodForm.getDescription());

        paymentMethodDocument1 = paymentMethodRepository.save(paymentMethodDocument1);
        paymentMethodDocument2 = paymentMethodRepository.save(paymentMethodDocument2);
        paymentMethodDocument3 = paymentMethodRepository.save(paymentMethodDocument3);
        paymentMethodDocument4 = paymentMethodRepository.save(paymentMethodDocument4);

    }

    @AfterEach
    private void destroy() {
        paymentMethodRepository.deleteById(paymentMethodDocument1.getId());
        paymentMethodRepository.deleteById(paymentMethodDocument2.getId());
        paymentMethodRepository.deleteById(paymentMethodDocument3.getId());
        paymentMethodRepository.deleteById(paymentMethodDocument4.getId());
    }

    @Test
    public void test_PaymentMethod_Post_ShouldReturn_201Response_And_NewPaymentMethodId_WhenPosted_WithValidPaymentMethodForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(CATEGORY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_PaymentMethod_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        paymentMethodForm.setName("");

        mvcResult = mockMvc.perform(post(CATEGORY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_PaymentMethod_Post_ShouldReturn_201Response_And_NewPaymentMethodId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        paymentMethodForm.setName("New PaymentMethod Name");
        paymentMethodForm.setDescription("");

        mvcResult = mockMvc.perform(post(CATEGORY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_PaymentMethod_Post_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenPosted_WithNoPaymentMethodForm() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(CATEGORY_URI)
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
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_PaymentMethodListNaturallyOrdered_WhenRequested_ForAllPaymentMethods() throws Exception {
        MvcResult mvcResult = null;
        Set<PaymentMethodVo> paymentMethodList = new TreeSet<>(Arrays.asList(paymentMethodVo1, paymentMethodVo2, paymentMethodVo3, paymentMethodVo4, paymentMethodVo5));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PaymentMethod_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PaymentMethod_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_PaymentMethod_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyPhoneNumberOnly(String phoneNumber) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("phoneNumber", phoneNumber))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_EmptyPaymentMethodList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_EmptyPaymentMethodList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_PaymentMethodListNaturallyOrdered_WhenRequested_ForPaymentMethods_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<PaymentMethodVo> paymentMethodList = new ArrayList<>(Arrays.asList(paymentMethodVo1, paymentMethodVo2, paymentMethodVo3, paymentMethodVo4));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "PaymentMethod"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_PaymentMethodListNaturallyOrdered_WhenRequested_ForPaymentMethods_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<PaymentMethodVo> paymentMethodList = new ArrayList<>(Arrays.asList(paymentMethodVo2));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("description", "PaymentMethod 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_PaymentMethodListNaturallyOrdered_WhenRequested_ForPaymentMethods_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<PaymentMethodVo> paymentMethodList = new TreeSet<>(Arrays.asList(paymentMethodVo1));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "PaymentMethod 1")
                        .queryParam("description", "PaymentMethod 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_PaymentMethodListNaturallyOrdered_WhenRequested_ForPaymentMethods_WithNameAndDescriptionAndPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        Set<PaymentMethodVo> paymentMethodList = new TreeSet<>(Arrays.asList(paymentMethodVo1));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "PaymentMethod 1")
                        .queryParam("description", "PaymentMethod 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_EmptyPaymentMethodList_WhenRequested_ForPaymentMethods_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<PaymentMethodVo> paymentMethodList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "PaymentMethod 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_EmptyPaymentMethodList_WhenRequested_ForPaymentMethods_WithAbsent_WithNameAndDescriptionAndPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        Set<PaymentMethodVo> paymentMethodList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "PaymentMethod 1")
                        .queryParam("description", UUID.randomUUID().toString())
                        .queryParam("phoneNumber", "1122334455"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo[].class).length);
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_PaymentMethodDetails_WhenRequested_ById() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(paymentMethodVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(paymentMethodVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_PaymentMethod_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = paymentMethodDocument3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getId());
        Assertions.assertEquals(paymentMethodVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getName());
        Assertions.assertEquals(paymentMethodVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getActive()));
    }

    @Test
    public void test_PaymentMethod_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(paymentMethodVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getId());
        Assertions.assertEquals(paymentMethodVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getName());
        Assertions.assertEquals(paymentMethodVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PaymentMethodVo.class).getActive()));
    }

    @Test
    public void test_PaymentMethod_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Delete_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = paymentMethodDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_PaymentMethod_Delete_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPaymentMethodDetails() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        paymentMethodForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_PaymentMethod_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenUpdatedBy_EmptyId_AndPaymentMethodDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Put_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndPaymentMethodDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_PaymentMethod_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenUpdated_ByInactiveId_AndPaymentMethodDetails() throws Exception {
        String id = paymentMethodDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_PaymentMethod_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoPaymentMethodDetails() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
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
    public void test_PaymentMethod_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        paymentMethodForm.setName("");

        mvcResult = mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        paymentMethodForm.setDescription("");

        mvcResult = mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(paymentMethodForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_PaymentMethod_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndEmptyPaymentMethodDetails() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new PaymentMethodForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_PaymentMethod_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPaymentMethodDetails() throws Exception {
        String id = paymentMethodDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(CATEGORY_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_PaymentMethod_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndPaymentMethodDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(CATEGORY_URI_BY_ID, " ")
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
    public void test_PaymentMethod_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByInvalidId_AndPaymentMethodDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(CATEGORY_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }*/

    @Test
    public void test_PaymentMethod_Patch_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndPaymentMethodDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(CATEGORY_URI_BY_ID, id)
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
    public void test_PaymentMethod_Patch_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoPaymentMethodDetails() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_PaymentMethod_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(CATEGORY_URI_BY_ID, id)
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
    public void test_PaymentMethod_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(CATEGORY_URI_BY_ID, id)
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
    public void test_PaymentMethod_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDefinitionOfPaymentMethodAttribute() throws Exception {
        String id = paymentMethodDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(CATEGORY_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

}
