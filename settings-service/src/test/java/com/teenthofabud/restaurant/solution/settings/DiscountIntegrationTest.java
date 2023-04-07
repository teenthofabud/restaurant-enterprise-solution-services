package com.teenthofabud.restaurant.solution.settings;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountDocument;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountForm;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountVo;
import com.teenthofabud.restaurant.solution.settings.discount.repository.DiscountRepository;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
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
public class DiscountIntegrationTest extends SettingsIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String DISCOUNT_URI = "/discount";
    private static final String DISCOUNT_URI_BY_ID = "/discount/{id}";
    private static final String DISCOUNT_URI_FILTER = "/discount/filter";

    private DiscountRepository discountRepository;

    @Autowired
    public void setDiscountRepository(DiscountRepository discountRepository) {
        this.discountRepository = discountRepository;
    }
    
    private DiscountForm discountForm;
    private DiscountVo discountVo1;
    private DiscountVo discountVo2;
    private DiscountVo discountVo3;
    private DiscountVo discountVo4;
    private DiscountVo discountVo5;
    private DiscountDocument discountDocument1;
    private DiscountDocument discountDocument2;
    private DiscountDocument discountDocument3;
    private DiscountDocument discountDocument4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Discount
         */

        discountForm = new DiscountForm();
        discountForm.setName("New Name");
        discountForm.setDescription("New Description");
        discountForm.setRate(7.9d);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"),
                new PatchOperationForm("replace", "/rate", "3.4"));

        discountDocument1 = new DiscountDocument();
        discountDocument1.setName("Discount 1 Name");
        discountDocument1.setDescription("Discount 1 Description");
        discountDocument1.setRate(4.78d);
        discountDocument1.setActive(Boolean.TRUE);

        discountDocument2 = new DiscountDocument();
        discountDocument2.setName("Discount 2 Name");
        discountDocument2.setDescription("Discount 2 Description");
        discountDocument2.setRate(6.8d);
        discountDocument2.setActive(Boolean.TRUE);

        discountDocument3 = new DiscountDocument();
        discountDocument3.setName("Discount 3 Name");
        discountDocument3.setDescription("Discount 3 Description");
        discountDocument3.setRate(6.99d);
        discountDocument3.setActive(Boolean.TRUE);

        discountDocument4 = new DiscountDocument();
        discountDocument4.setName("Discount 4 Name");
        discountDocument4.setDescription("Discount 4 Description");
        discountDocument4.setRate(12.66d);
        discountDocument4.setActive(Boolean.FALSE);

        discountDocument1 = discountRepository.save(discountDocument1);

        discountVo1 = new DiscountVo();
        discountVo1.setId(discountDocument1.getId().toString());
        discountVo1.setName(discountDocument1.getName());
        discountVo1.setRate(discountDocument1.getRate());
        discountVo1.setDescription(discountDocument1.getDescription());

        discountDocument2 = discountRepository.save(discountDocument2);

        discountVo2 = new DiscountVo();
        discountVo2.setId(discountDocument2.getId().toString());
        discountVo2.setName(discountDocument2.getName());
        discountVo2.setRate(discountDocument2.getRate());
        discountVo2.setDescription(discountDocument2.getDescription());

        discountDocument3 = discountRepository.save(discountDocument3);

        discountVo3 = new DiscountVo();
        discountVo3.setId(discountDocument3.getId().toString());
        discountVo3.setName(discountDocument3.getName());
        discountVo3.setRate(discountDocument3.getRate());
        discountVo3.setDescription(discountDocument3.getDescription());

        discountDocument4 = discountRepository.save(discountDocument4);

        discountVo4 = new DiscountVo();
        discountVo4.setId(discountDocument4.getId().toString());
        discountVo4.setName(discountDocument4.getName());
        discountVo4.setRate(discountDocument4.getRate());
        discountVo4.setDescription(discountDocument4.getDescription());

        discountVo5 = new DiscountVo();
        discountVo5.setId(UUID.randomUUID().toString());
        discountVo5.setName(discountForm.getName());
        discountVo5.setRate(discountForm.getRate());
        discountVo5.setDescription(discountForm.getDescription());

        discountDocument1 = discountRepository.save(discountDocument1);
        discountDocument2 = discountRepository.save(discountDocument2);
        discountDocument3 = discountRepository.save(discountDocument3);
        discountDocument4 = discountRepository.save(discountDocument4);

    }

    @AfterEach
    private void destroy() {
        discountRepository.deleteById(discountDocument1.getId());
        discountRepository.deleteById(discountDocument2.getId());
        discountRepository.deleteById(discountDocument3.getId());
        discountRepository.deleteById(discountDocument4.getId());
    }

    @Test
    public void test_Discount_Post_ShouldReturn_201Response_And_NewDiscountId_WhenPosted_WithValidDiscountForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(DISCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Discount_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        discountForm.setName("");

        mvcResult = mockMvc.perform(post(DISCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Discount_Post_ShouldReturn_201Response_And_NewDiscountId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        discountForm.setName("New Discount Name");
        discountForm.setDescription("");

        mvcResult = mockMvc.perform(post(DISCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Discount_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidRate() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        discountForm.setRate(-9.8d);

        mvcResult = mockMvc.perform(post(DISCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndEmptyRate() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        discountForm.setRate(null);

        mvcResult = mockMvc.perform(post(DISCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@Test
    public void test_Discount_Post_ShouldReturn_201Response_And_NewDiscountId_WhenPosted_WithEmptyRate() throws Exception {
        MvcResult mvcResult = null;
        discountForm.setName("New Discount Name");
        discountForm.setRate(null);

        mvcResult = mockMvc.perform(post(DISCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Discount_Post_ShouldReturn_201Response_And_NewDiscountId_WhenPosted_WithInvalidRate() throws Exception {
        MvcResult mvcResult = null;
        discountForm.setName("New Discount Name");
        discountForm.setRate(null);

        mvcResult = mockMvc.perform(post(DISCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }*/

    @Test
    public void test_Discount_Post_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenPosted_WithNoDiscountForm() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(DISCOUNT_URI)
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
    public void test_Discount_Get_ShouldReturn_200Response_And_DiscountListNaturallyOrdered_WhenRequested_ForAllDiscounts() throws Exception {
        MvcResult mvcResult = null;
        Set<DiscountVo> discountList = new TreeSet<>(Arrays.asList(discountVo1, discountVo2, discountVo3, discountVo4));

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(discountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Discount_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Discount_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_EmptyDiscountList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo[].class).length);
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_EmptyDiscountList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo[].class).length);
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_DiscountListNaturallyOrdered_WhenRequested_ForDiscounts_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<DiscountVo> discountList = new ArrayList<>(Arrays.asList(discountVo1, discountVo2, discountVo3, discountVo4));

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER)
                        .queryParam("name", "Discount"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(discountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo[].class).length);
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_DiscountListNaturallyOrdered_WhenRequested_ForDiscounts_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<DiscountVo> discountList = new ArrayList<>(Arrays.asList(discountVo2));

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER)
                        .queryParam("description", "Discount 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(discountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo[].class).length);
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_DiscountListNaturallyOrdered_WhenRequested_ForDiscounts_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<DiscountVo> discountList = new TreeSet<>(Arrays.asList(discountVo1));

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER)
                        .queryParam("name", "Discount 1")
                        .queryParam("description", "Discount 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(discountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo[].class).length);
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_EmptyDiscountList_WhenRequested_ForDiscounts_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<DiscountVo> discountList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_FILTER)
                        .queryParam("name", "Discount 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(discountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo[].class).length);
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_DiscountDetails_WhenRequested_ById() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(discountVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(discountVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Discount_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = discountDocument3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(discountVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getId());
        Assertions.assertEquals(discountVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getName());
        Assertions.assertEquals(discountVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getDescription());
        Assertions.assertEquals(discountVo1.getRate(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getRate());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getActive()));
    }

    @Test
    public void test_Discount_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DISCOUNT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(discountVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getId());
        Assertions.assertEquals(discountVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getName());
        Assertions.assertEquals(discountVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getDescription());
        Assertions.assertEquals(discountVo1.getRate(), om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getRate());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DiscountVo.class).getActive()));
    }

    @Test
    public void test_Discount_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DISCOUNT_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Delete_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = discountDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Discount_Delete_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDiscountDetails() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        discountForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Discount_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenUpdatedBy_EmptyId_AndDiscountDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Put_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndDiscountDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Discount_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenUpdated_ByInactiveId_AndDiscountDetails() throws Exception {
        String id = discountDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Discount_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoDiscountDetails() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
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
    public void test_Discount_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        discountForm.setName("");

        mvcResult = mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        discountForm.setDescription("");

        mvcResult = mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidRate() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        discountForm.setRate(-9.8d);

        mvcResult = mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(discountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Discount_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndEmptyDiscountDetails() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(DISCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new DiscountForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Discount_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDiscountDetails() throws Exception {
        String id = discountDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Discount_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenUpdated_ByEmptyId_AndDiscountDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(DISCOUNT_URI_BY_ID, " ")
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
    public void test_Discount_Patch_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndDiscountDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id)
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
    public void test_Discount_Patch_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoDiscountDetails() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Discount_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id)
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
    public void test_Discount_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidRate() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "rate";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/rate", "x"));

        mvcResult = mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id)
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
    public void test_Discount_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id)
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
    public void test_Discount_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyRate() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/rate", " "));

        mvcResult = mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id)
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
    public void test_Discount_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDefinitionOfDiscountAttribute() throws Exception {
        String id = discountDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(DISCOUNT_URI_BY_ID, id)
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
