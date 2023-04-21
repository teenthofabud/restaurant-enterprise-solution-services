package com.teenthofabud.restaurant.solution.settings;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerDocument;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerForm;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerVo;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.repository.DeliveryPartnerRepository;
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
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class DeliveryPartnerIntegrationTest extends SettingsIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String DELIVERY_PARTNER_URI = "/deliverypartner";
    private static final String DELIVERY_PARTNER_URI_BY_ID = "/deliverypartner/{id}";
    private static final String DELIVERY_PARTNER_URI_FILTER = "/deliverypartner/filter";

    private DeliveryPartnerRepository deliveryPartnerRepository;

    @Autowired
    public void setDeliveryPartnerRepository(DeliveryPartnerRepository deliveryPartnerRepository) {
        this.deliveryPartnerRepository = deliveryPartnerRepository;
    }
    
    private DeliveryPartnerForm deliveryPartnerForm;
    private DeliveryPartnerVo deliveryPartnerVo1;
    private DeliveryPartnerVo deliveryPartnerVo2;
    private DeliveryPartnerVo deliveryPartnerVo3;
    private DeliveryPartnerVo deliveryPartnerVo4;
    private DeliveryPartnerVo deliveryPartnerVo5;
    private DeliveryPartnerDocument deliveryPartnerDocument1;
    private DeliveryPartnerDocument deliveryPartnerDocument2;
    private DeliveryPartnerDocument deliveryPartnerDocument3;
    private DeliveryPartnerDocument deliveryPartnerDocument4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * DeliveryPartner
         */

        deliveryPartnerForm = new DeliveryPartnerForm();
        deliveryPartnerForm.setName("New Name");
        deliveryPartnerForm.setDescription("New Description");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"));

        deliveryPartnerDocument1 = new DeliveryPartnerDocument();
        deliveryPartnerDocument1.setName("DeliveryPartner 1 Name");
        deliveryPartnerDocument1.setDescription("DeliveryPartner 1 Description");
        deliveryPartnerDocument1.setActive(Boolean.TRUE);

        deliveryPartnerDocument2 = new DeliveryPartnerDocument();
        deliveryPartnerDocument2.setName("DeliveryPartner 2 Name");
        deliveryPartnerDocument2.setDescription("DeliveryPartner 2 Description");
        deliveryPartnerDocument2.setActive(Boolean.TRUE);

        deliveryPartnerDocument3 = new DeliveryPartnerDocument();
        deliveryPartnerDocument3.setName("DeliveryPartner 3 Name");
        deliveryPartnerDocument3.setDescription("DeliveryPartner 3 Description");
        deliveryPartnerDocument3.setActive(Boolean.TRUE);

        deliveryPartnerDocument4 = new DeliveryPartnerDocument();
        deliveryPartnerDocument4.setName("DeliveryPartner 4 Name");
        deliveryPartnerDocument4.setDescription("DeliveryPartner 4 Description");
        deliveryPartnerDocument4.setActive(Boolean.FALSE);

        deliveryPartnerDocument1 = deliveryPartnerRepository.save(deliveryPartnerDocument1);

        deliveryPartnerVo1 = new DeliveryPartnerVo();
        deliveryPartnerVo1.setId(deliveryPartnerDocument1.getId().toString());
        deliveryPartnerVo1.setName(deliveryPartnerDocument1.getName());
        deliveryPartnerVo1.setDescription(deliveryPartnerDocument1.getDescription());

        deliveryPartnerDocument2 = deliveryPartnerRepository.save(deliveryPartnerDocument2);

        deliveryPartnerVo2 = new DeliveryPartnerVo();
        deliveryPartnerVo2.setId(deliveryPartnerDocument2.getId().toString());
        deliveryPartnerVo2.setName(deliveryPartnerDocument2.getName());
        deliveryPartnerVo2.setDescription(deliveryPartnerDocument2.getDescription());

        deliveryPartnerDocument3 = deliveryPartnerRepository.save(deliveryPartnerDocument3);

        deliveryPartnerVo3 = new DeliveryPartnerVo();
        deliveryPartnerVo3.setId(deliveryPartnerDocument3.getId().toString());
        deliveryPartnerVo3.setName(deliveryPartnerDocument3.getName());
        deliveryPartnerVo3.setDescription(deliveryPartnerDocument3.getDescription());

        deliveryPartnerDocument4 = deliveryPartnerRepository.save(deliveryPartnerDocument4);

        deliveryPartnerVo4 = new DeliveryPartnerVo();
        deliveryPartnerVo4.setId(deliveryPartnerDocument4.getId().toString());
        deliveryPartnerVo4.setName(deliveryPartnerDocument4.getName());
        deliveryPartnerVo4.setDescription(deliveryPartnerDocument4.getDescription());

        deliveryPartnerVo5 = new DeliveryPartnerVo();
        deliveryPartnerVo5.setId(UUID.randomUUID().toString());
        deliveryPartnerVo5.setName(deliveryPartnerForm.getName());
        deliveryPartnerVo5.setDescription(deliveryPartnerForm.getDescription());

        deliveryPartnerDocument1 = deliveryPartnerRepository.save(deliveryPartnerDocument1);
        deliveryPartnerDocument2 = deliveryPartnerRepository.save(deliveryPartnerDocument2);
        deliveryPartnerDocument3 = deliveryPartnerRepository.save(deliveryPartnerDocument3);
        deliveryPartnerDocument4 = deliveryPartnerRepository.save(deliveryPartnerDocument4);

    }

    @AfterEach
    private void destroy() {
        deliveryPartnerRepository.deleteById(deliveryPartnerDocument1.getId());
        deliveryPartnerRepository.deleteById(deliveryPartnerDocument2.getId());
        deliveryPartnerRepository.deleteById(deliveryPartnerDocument3.getId());
        deliveryPartnerRepository.deleteById(deliveryPartnerDocument4.getId());
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_DeliveryPartnerListNaturallyOrdered_WhenRequested_ForAllDeliveryPartners() throws Exception {
        MvcResult mvcResult = null;
        DeliveryPartnerVo deliveryPartnerVo6 = new DeliveryPartnerVo();
        deliveryPartnerVo6.setId(UUID.randomUUID().toString());
        deliveryPartnerVo6.setName("Another Name");
        deliveryPartnerVo6.setDescription("");
        List<DeliveryPartnerVo> deliveryPartnerList = Arrays.asList(deliveryPartnerVo1, deliveryPartnerVo2, deliveryPartnerVo3, deliveryPartnerVo4);
        //List<DeliveryPartnerVo> deliveryPartnerList = Arrays.asList(deliveryPartnerVo1, deliveryPartnerVo2, deliveryPartnerVo3, deliveryPartnerVo4, deliveryPartnerVo5, deliveryPartnerVo6);

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryPartnerList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo[].class).length);
    }

    @Test
    public void test_DeliveryPartner_Post_ShouldReturn_201Response_And_NewDeliveryPartnerId_WhenPosted_WithValidDeliveryPartnerForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(DELIVERY_PARTNER_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_DeliveryPartner_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        deliveryPartnerForm.setName("");

        mvcResult = mockMvc.perform(post(DELIVERY_PARTNER_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_DeliveryPartner_Post_ShouldReturn_201Response_And_NewDeliveryPartnerId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        deliveryPartnerForm.setName("Another Name");
        deliveryPartnerForm.setDescription("");

        mvcResult = mockMvc.perform(post(DELIVERY_PARTNER_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_DeliveryPartner_Post_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenPosted_WithNoDeliveryPartnerForm() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(DELIVERY_PARTNER_URI)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_DeliveryPartner_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_DeliveryPartner_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_EmptyDeliveryPartnerList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo[].class).length);
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_EmptyDeliveryPartnerList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo[].class).length);
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_DeliveryPartnerListNaturallyOrdered_WhenRequested_ForDeliveryPartners_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<DeliveryPartnerVo> deliveryPartnerList = new ArrayList<>(Arrays.asList(deliveryPartnerVo1, deliveryPartnerVo2, deliveryPartnerVo3, deliveryPartnerVo4));

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER)
                        .queryParam("name", "DeliveryPartner"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryPartnerList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo[].class).length);
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_DeliveryPartnerListNaturallyOrdered_WhenRequested_ForDeliveryPartners_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<DeliveryPartnerVo> deliveryPartnerList = new ArrayList<>(Arrays.asList(deliveryPartnerVo2));

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER)
                        .queryParam("description", "DeliveryPartner 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryPartnerList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo[].class).length);
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_DeliveryPartnerListNaturallyOrdered_WhenRequested_ForDeliveryPartners_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<DeliveryPartnerVo> deliveryPartnerList = Arrays.asList(deliveryPartnerVo1);

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER)
                        .queryParam("name", "DeliveryPartner 1")
                        .queryParam("description", "DeliveryPartner 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryPartnerList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo[].class).length);
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_EmptyDeliveryPartnerList_WhenRequested_ForDeliveryPartners_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<DeliveryPartnerVo> deliveryPartnerList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_FILTER)
                        .queryParam("name", "DeliveryPartner 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryPartnerList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo[].class).length);
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_DeliveryPartnerDetails_WhenRequested_ById() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(deliveryPartnerVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(deliveryPartnerVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_DeliveryPartner_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = deliveryPartnerDocument3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryPartnerVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getId());
        Assertions.assertEquals(deliveryPartnerVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getName());
        Assertions.assertEquals(deliveryPartnerVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getDescription());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getActive()));
    }

    @Test
    public void test_DeliveryPartner_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DELIVERY_PARTNER_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deliveryPartnerVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getId());
        Assertions.assertEquals(deliveryPartnerVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getName());
        Assertions.assertEquals(deliveryPartnerVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeliveryPartnerVo.class).getActive()));
    }

    @Test
    public void test_DeliveryPartner_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DELIVERY_PARTNER_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Delete_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = deliveryPartnerDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_DeliveryPartner_Delete_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDeliveryPartnerDetails() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        deliveryPartnerForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_DeliveryPartner_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenUpdatedBy_EmptyId_AndDeliveryPartnerDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Put_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndDeliveryPartnerDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_DeliveryPartner_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenUpdated_ByInactiveId_AndDeliveryPartnerDetails() throws Exception {
        String id = deliveryPartnerDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_DeliveryPartner_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoDeliveryPartnerDetails() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
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
    public void test_DeliveryPartner_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        deliveryPartnerForm.setName("");

        mvcResult = mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        deliveryPartnerForm.setDescription("");

        mvcResult = mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deliveryPartnerForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_DeliveryPartner_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndEmptyDeliveryPartnerDetails() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new DeliveryPartnerForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_DeliveryPartner_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDeliveryPartnerDetails() throws Exception {
        String id = deliveryPartnerDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_DeliveryPartner_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndDeliveryPartnerDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, " ")
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
    public void test_DeliveryPartner_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByInvalidId_AndDeliveryPartnerDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, id)
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
    public void test_DeliveryPartner_Patch_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndDeliveryPartnerDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, id)
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
    public void test_DeliveryPartner_Patch_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoDeliveryPartnerDetails() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_DeliveryPartner_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, id)
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
    public void test_DeliveryPartner_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, id)
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
    public void test_DeliveryPartner_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDefinitionOfDeliveryPartnerAttribute() throws Exception {
        String id = deliveryPartnerDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(DELIVERY_PARTNER_URI_BY_ID, id)
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
