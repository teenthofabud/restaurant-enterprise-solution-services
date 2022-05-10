package com.teenthofabud.restaurant.solution.reservation;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryForm;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
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
public class CategoryIntegrationTest extends ReservationIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String CATEGORY_URI = "/category";
    private static final String CATEGORY_URI_BY_ID = "/category/{id}";
    private static final String CATEGORY_URI_FILTER = "/category/filter";

    private CategoryRepository categoryRepository;

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }
    
    private CategoryForm categoryForm;
    private CategoryVo categoryVo1;
    private CategoryVo categoryVo2;
    private CategoryVo categoryVo3;
    private CategoryVo categoryVo4;
    private CategoryVo categoryVo5;
    private CategoryDocument categoryDocument1;
    private CategoryDocument categoryDocument2;
    private CategoryDocument categoryDocument3;
    private CategoryDocument categoryDocument4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Category
         */

        categoryForm = new CategoryForm();
        categoryForm.setName("New Name");
        categoryForm.setDescription("New Description");
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"));

        categoryDocument1 = new CategoryDocument();
        categoryDocument1.setName("Category 1 Name");
        categoryDocument1.setDescription("Category 1 Description");
        categoryDocument1.setActive(Boolean.TRUE);

        categoryDocument2 = new CategoryDocument();
        categoryDocument2.setName("Category 2 Name");
        categoryDocument2.setDescription("Category 2 Description");
        categoryDocument2.setActive(Boolean.TRUE);

        categoryDocument3 = new CategoryDocument();
        categoryDocument3.setName("Category 3 Name");
        categoryDocument3.setDescription("Category 3 Description");
        categoryDocument3.setActive(Boolean.TRUE);

        categoryDocument4 = new CategoryDocument();
        categoryDocument4.setName("Category 4 Name");
        categoryDocument4.setDescription("Category 4 Description");
        categoryDocument4.setActive(Boolean.FALSE);

        categoryDocument1 = categoryRepository.save(categoryDocument1);

        categoryVo1 = new CategoryVo();
        categoryVo1.setId(categoryDocument1.getId().toString());
        categoryVo1.setName(categoryDocument1.getName());
        categoryVo1.setDescription(categoryDocument1.getDescription());

        categoryDocument2 = categoryRepository.save(categoryDocument2);

        categoryVo2 = new CategoryVo();
        categoryVo2.setId(categoryDocument2.getId().toString());
        categoryVo2.setName(categoryDocument2.getName());
        categoryVo2.setDescription(categoryDocument2.getDescription());

        categoryDocument3 = categoryRepository.save(categoryDocument3);

        categoryVo3 = new CategoryVo();
        categoryVo3.setId(categoryDocument3.getId().toString());
        categoryVo3.setName(categoryDocument3.getName());
        categoryVo3.setDescription(categoryDocument3.getDescription());

        categoryDocument4 = categoryRepository.save(categoryDocument4);

        categoryVo4 = new CategoryVo();
        categoryVo4.setId(categoryDocument4.getId().toString());
        categoryVo4.setName(categoryDocument4.getName());
        categoryVo4.setDescription(categoryDocument4.getDescription());

        categoryVo5 = new CategoryVo();
        categoryVo5.setId(UUID.randomUUID().toString());
        categoryVo5.setName(categoryForm.getName());
        categoryVo5.setDescription(categoryForm.getDescription());

        categoryDocument1 = categoryRepository.save(categoryDocument1);
        categoryDocument2 = categoryRepository.save(categoryDocument2);
        categoryDocument3 = categoryRepository.save(categoryDocument3);
        categoryDocument4 = categoryRepository.save(categoryDocument4);

    }

    @AfterEach
    private void destroy() {
        categoryRepository.deleteById(categoryDocument1.getId());
        categoryRepository.deleteById(categoryDocument2.getId());
        categoryRepository.deleteById(categoryDocument3.getId());
        categoryRepository.deleteById(categoryDocument4.getId());
    }

    @Test
    public void test_Category_Post_ShouldReturn_201Response_And_NewCategoryId_WhenPosted_WithValidCategoryForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(CATEGORY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Category_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        categoryForm.setName("");

        mvcResult = mockMvc.perform(post(CATEGORY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Category_Post_ShouldReturn_201Response_And_NewCategoryId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        categoryForm.setName("New Category Name");
        categoryForm.setDescription("");

        mvcResult = mockMvc.perform(post(CATEGORY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Category_Post_ShouldReturn_422Response_And_ErrorCode_RES_RESERVATION_003_WhenPosted_WithNoCategoryForm() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForAllCategorys() throws Exception {
        MvcResult mvcResult = null;
        Set<CategoryVo> categoryList = new TreeSet<>(Arrays.asList(categoryVo1, categoryVo2, categoryVo3, categoryVo4));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_EmptyCategoryList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_EmptyCategoryList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForCategorys_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<CategoryVo> categoryList = new ArrayList<>(Arrays.asList(categoryVo1, categoryVo2, categoryVo3, categoryVo4, categoryVo5));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "Category"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForCategorys_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<CategoryVo> categoryList = new ArrayList<>(Arrays.asList(categoryVo2));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("description", "Category 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForCategorys_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<CategoryVo> categoryList = new TreeSet<>(Arrays.asList(categoryVo1));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "Category 1")
                        .queryParam("description", "Category 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_EmptyCategoryList_WhenRequested_ForCategorys_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<CategoryVo> categoryList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "Category 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryDetails_WhenRequested_ById() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(categoryVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(categoryVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
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
    public void test_Category_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = categoryDocument3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getId());
        Assertions.assertEquals(categoryVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getName());
        Assertions.assertEquals(categoryVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getActive()));
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getId());
        Assertions.assertEquals(categoryVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getName());
        Assertions.assertEquals(categoryVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getActive()));
    }

    @Test
    public void test_Category_Delete_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Delete_ShouldReturn_422Response_And_ErrorCode_RES_RESERVATION_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Delete_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = categoryDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Category_Delete_ShouldReturn_404Response_And_ErrorCode_RES_RESERVATION_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndCategoryDetails() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        categoryForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenUpdatedBy_EmptyId_AndCategoryDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Category_Put_ShouldReturn_404Response_And_ErrorCode_RES_RESERVATION_002_WhenUpdated_ByAbsentId_AndCategoryDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_005_WhenUpdated_ByInactiveId_AndCategoryDetails() throws Exception {
        String id = categoryDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Category_Put_ShouldReturn_422Response_And_ErrorCode_RES_RESERVATION_003_WhenUpdated_ById_AndNoCategoryDetails() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        categoryForm.setName("");

        mvcResult = mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        categoryForm.setDescription("");

        mvcResult = mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(categoryForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Category_Put_ShouldReturn_422Response_And_ErrorCode_RES_RESERVATION_003_WhenUpdated_ById_AndEmptyCategoryDetails() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(CATEGORY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new CategoryForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Category_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndCategoryDetails() throws Exception {
        String id = categoryDocument4.getId().toString();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenUpdated_ByEmptyId_AndCategoryDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
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

    @Test
    public void test_Category_Patch_ShouldReturn_404Response_And_ErrorCode_RES_RESERVATION_002_WhenUpdated_ByAbsentId_AndCategoryDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_422Response_And_ErrorCode_RES_RESERVATION_003_WhenUpdated_ById_AndNoCategoryDetails() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = categoryDocument1.getId().toString();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyRate() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/rate", " "));

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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_RESERVATION_001_WhenRequested_ById_AndInvalidDefinitionOfCategoryAttribute() throws Exception {
        String id = categoryDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
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

    @Override
    public String getSimulationBaseLocation() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("simulation base location not available");
    }

    @Override
    public Integer getServicePort() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("service port not available");
    }

    @Override
    public String[] getSimulationFilePaths() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("simulation file path not available");
    }
}
