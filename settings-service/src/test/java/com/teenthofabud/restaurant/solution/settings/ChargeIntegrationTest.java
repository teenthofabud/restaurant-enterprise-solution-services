package com.teenthofabud.restaurant.solution.settings;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeDocument;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeForm;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeVo;
import com.teenthofabud.restaurant.solution.settings.charge.repository.ChargeRepository;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;
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
public class ChargeIntegrationTest extends SettingsIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String CHARGE_URI = "/charge";
    private static final String CHARGE_URI_BY_ID = "/charge/{id}";
    private static final String CHARGE_URI_FILTER = "/charge/filter";

    private ChargeRepository chargeRepository;

    @Autowired
    public void setChargeRepository(ChargeRepository chargeRepository) {
        this.chargeRepository = chargeRepository;
    }
    
    private ChargeForm chargeForm;
    private ChargeVo chargeVo1;
    private ChargeVo chargeVo2;
    private ChargeVo chargeVo3;
    private ChargeVo chargeVo4;
    private ChargeVo chargeVo5;
    private ChargeDocument chargeDocument1;
    private ChargeDocument chargeDocument2;
    private ChargeDocument chargeDocument3;
    private ChargeDocument chargeDocument4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Charge
         */

        chargeForm = new ChargeForm();
        chargeForm.setName("New Name");
        chargeForm.setDescription("New Description");
        chargeForm.setRate(7.9d);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"),
                new PatchOperationForm("replace", "/rate", "3.4"));

        chargeDocument1 = new ChargeDocument();
        chargeDocument1.setName("Charge 1 Name");
        chargeDocument1.setDescription("Charge 1 Description");
        chargeDocument1.setRate(4.78d);
        chargeDocument1.setActive(Boolean.TRUE);

        chargeDocument2 = new ChargeDocument();
        chargeDocument2.setName("Charge 2 Name");
        chargeDocument2.setDescription("Charge 2 Description");
        chargeDocument2.setRate(6.8d);
        chargeDocument2.setActive(Boolean.TRUE);

        chargeDocument3 = new ChargeDocument();
        chargeDocument3.setName("Charge 3 Name");
        chargeDocument3.setDescription("Charge 3 Description");
        chargeDocument3.setRate(6.99d);
        chargeDocument3.setActive(Boolean.TRUE);

        chargeDocument4 = new ChargeDocument();
        chargeDocument4.setName("Charge 4 Name");
        chargeDocument4.setDescription("Charge 4 Description");
        chargeDocument4.setRate(12.66d);
        chargeDocument4.setActive(Boolean.FALSE);

        chargeDocument1 = chargeRepository.save(chargeDocument1);

        chargeVo1 = new ChargeVo();
        chargeVo1.setId(chargeDocument1.getId().toString());
        chargeVo1.setName(chargeDocument1.getName());
        chargeVo1.setRate(chargeDocument1.getRate());
        chargeVo1.setDescription(chargeDocument1.getDescription());

        chargeDocument2 = chargeRepository.save(chargeDocument2);

        chargeVo2 = new ChargeVo();
        chargeVo2.setId(chargeDocument2.getId().toString());
        chargeVo2.setName(chargeDocument2.getName());
        chargeVo2.setRate(chargeDocument2.getRate());
        chargeVo2.setDescription(chargeDocument2.getDescription());

        chargeDocument3 = chargeRepository.save(chargeDocument3);

        chargeVo3 = new ChargeVo();
        chargeVo3.setId(chargeDocument3.getId().toString());
        chargeVo3.setName(chargeDocument3.getName());
        chargeVo3.setRate(chargeDocument3.getRate());
        chargeVo3.setDescription(chargeDocument3.getDescription());

        chargeDocument4 = chargeRepository.save(chargeDocument4);

        chargeVo4 = new ChargeVo();
        chargeVo4.setId(chargeDocument4.getId().toString());
        chargeVo4.setName(chargeDocument4.getName());
        chargeVo4.setRate(chargeDocument4.getRate());
        chargeVo4.setDescription(chargeDocument4.getDescription());

        chargeVo5 = new ChargeVo();
        chargeVo5.setId(UUID.randomUUID().toString());
        chargeVo5.setName(chargeForm.getName());
        chargeVo5.setRate(chargeForm.getRate());
        chargeVo5.setDescription(chargeForm.getDescription());

        chargeDocument1 = chargeRepository.save(chargeDocument1);
        chargeDocument2 = chargeRepository.save(chargeDocument2);
        chargeDocument3 = chargeRepository.save(chargeDocument3);
        chargeDocument4 = chargeRepository.save(chargeDocument4);

    }

    @AfterEach
    private void destroy() {
        chargeRepository.deleteById(chargeDocument1.getId());
        chargeRepository.deleteById(chargeDocument2.getId());
        chargeRepository.deleteById(chargeDocument3.getId());
        chargeRepository.deleteById(chargeDocument4.getId());
    }

    @Test
    public void test_Charge_Post_ShouldReturn_201Response_And_NewChargeId_WhenPosted_WithValidChargeForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Charge_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        chargeForm.setName("");

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Charge_Post_ShouldReturn_201Response_And_NewChargeId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        chargeForm.setName("New Charge Name");
        chargeForm.setDescription("");

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Charge_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidRate() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        chargeForm.setRate(-9.8d);

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndEmptyRate() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        chargeForm.setRate(null);

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@Test
    public void test_Charge_Post_ShouldReturn_201Response_And_NewChargeId_WhenPosted_WithEmptyRate() throws Exception {
        MvcResult mvcResult = null;
        chargeForm.setName("New Charge Name");
        chargeForm.setRate(null);

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Charge_Post_ShouldReturn_201Response_And_NewChargeId_WhenPosted_WithInvalidRate() throws Exception {
        MvcResult mvcResult = null;
        chargeForm.setName("New Charge Name");
        chargeForm.setRate(null);

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }*/

    @Test
    public void test_Charge_Post_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenPosted_WithNoChargeForm() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(CHARGE_URI)
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
    public void test_Charge_Get_ShouldReturn_200Response_And_ChargeListNaturallyOrdered_WhenRequested_ForAllCharges() throws Exception {
        MvcResult mvcResult = null;
        Set<ChargeVo> chargeList = new TreeSet<>(Arrays.asList(chargeVo1, chargeVo2, chargeVo3, chargeVo4));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(chargeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Charge_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Charge_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_EmptyChargeList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo[].class).length);
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_EmptyChargeList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo[].class).length);
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_ChargeListNaturallyOrdered_WhenRequested_ForCharges_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<ChargeVo> chargeList = new ArrayList<>(Arrays.asList(chargeVo1, chargeVo2, chargeVo3, chargeVo4));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("name", "Charge"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(chargeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo[].class).length);
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_ChargeListNaturallyOrdered_WhenRequested_ForCharges_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<ChargeVo> chargeList = new ArrayList<>(Arrays.asList(chargeVo2));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("description", "Charge 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(chargeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo[].class).length);
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_ChargeListNaturallyOrdered_WhenRequested_ForCharges_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<ChargeVo> chargeList = new TreeSet<>(Arrays.asList(chargeVo1));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("name", "Charge 1")
                        .queryParam("description", "Charge 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(chargeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo[].class).length);
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_EmptyChargeList_WhenRequested_ForCharges_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<ChargeVo> chargeList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("name", "Charge 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(chargeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo[].class).length);
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_ChargeDetails_WhenRequested_ById() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(chargeVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(chargeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Charge_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = chargeDocument3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(chargeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getId());
        Assertions.assertEquals(chargeVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getName());
        Assertions.assertEquals(chargeVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getDescription());
        Assertions.assertEquals(chargeVo1.getRate(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getRate());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getActive()));
    }

    @Test
    public void test_Charge_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(chargeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getId());
        Assertions.assertEquals(chargeVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getName());
        Assertions.assertEquals(chargeVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getDescription());
        Assertions.assertEquals(chargeVo1.getRate(), om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getRate());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ChargeVo.class).getActive()));
    }

    @Test
    public void test_Charge_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Delete_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = chargeDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Charge_Delete_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndChargeDetails() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        chargeForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Charge_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenUpdatedBy_EmptyId_AndChargeDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Put_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndChargeDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Charge_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenUpdated_ByInactiveId_AndChargeDetails() throws Exception {
        String id = chargeDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Charge_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoChargeDetails() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
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
    public void test_Charge_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        chargeForm.setName("");

        mvcResult = mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        chargeForm.setDescription("");

        mvcResult = mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidRate() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        chargeForm.setRate(-9.8d);

        mvcResult = mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(chargeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Charge_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndEmptyChargeDetails() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new ChargeForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Charge_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndChargeDetails() throws Exception {
        String id = chargeDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Charge_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenUpdated_ByEmptyId_AndChargeDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, " ")
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
    public void test_Charge_Patch_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndChargeDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Charge_Patch_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoChargeDetails() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Charge_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Charge_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidRate() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/rate", "x"));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Charge_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Charge_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyRate() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/rate", " "));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Charge_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDefinitionOfChargeAttribute() throws Exception {
        String id = chargeDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
