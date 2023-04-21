package com.teenthofabud.restaurant.solution.menu;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryForm;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.item.data.VegeterianStatus;
import com.teenthofabud.restaurant.solution.menu.item.repository.ItemRepository;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class CategoryIntegrationTest extends MenuIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String CATEGORY_URI = "/category";
    private static final String CATEGORY_URI_BY_ID = "/category/{id}";
    private static final String CATEGORY_URI_FILTER = "/category/filter";

    private CategoryRepository categoryRepository;
    private ItemRepository itemRepository;

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    @Autowired
    public void setItemRepository(ItemRepository itemRepository) {
        this.itemRepository = itemRepository;
    }

    private CategoryForm categoryForm;
    private CategoryVo categoryVo1;
    private CategoryVo categoryVo2;
    private CategoryVo categoryVo3;
    private CategoryVo categoryVo4;
    private CategoryEntity categoryEntity1;
    private CategoryEntity categoryEntity2;
    private CategoryEntity categoryEntity3;
    private CategoryEntity categoryEntity4;

    private List<PatchOperationForm> patches;

    private ItemVo itemVo1;
    private ItemVo itemVo2;
    private ItemVo itemVo3;
    private ItemVo itemVo4;
    private ItemVo itemVo5;
    private ItemEntity itemEntity1;
    private ItemEntity itemEntity2;
    private ItemEntity itemEntity3;
    private ItemEntity itemEntity4;
    private ItemEntity itemEntity5;

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

        categoryEntity1 = new CategoryEntity();
        categoryEntity1.setName("Category 1 Name");
        categoryEntity1.setDescription("Category 1 Description");
        categoryEntity1.setActive(Boolean.TRUE);

        categoryEntity2 = new CategoryEntity();
        categoryEntity2.setName("Category 2 Name");
        categoryEntity2.setDescription("Category 2 Description");
        categoryEntity2.setActive(Boolean.TRUE);

        categoryEntity3 = new CategoryEntity();
        categoryEntity3.setName("Category 3 Name");
        categoryEntity3.setDescription("Category 3 Description");
        categoryEntity3.setActive(Boolean.TRUE);

        categoryEntity4 = new CategoryEntity();
        categoryEntity4.setName("Category 4 Name");
        categoryEntity4.setDescription("Category 4 Description");
        categoryEntity4.setActive(Boolean.FALSE);

        categoryEntity1 = categoryRepository.save(categoryEntity1);

        categoryVo1 = new CategoryVo();
        categoryVo1.setId(categoryEntity1.getId().toString());
        categoryVo1.setName(categoryEntity1.getName());
        categoryVo1.setDescription(categoryEntity1.getDescription());

        categoryEntity2 = categoryRepository.save(categoryEntity2);

        categoryVo2 = new CategoryVo();
        categoryVo2.setId(categoryEntity2.getId().toString());
        categoryVo2.setName(categoryEntity2.getName());
        categoryVo2.setDescription(categoryEntity2.getDescription());

        categoryEntity3 = categoryRepository.save(categoryEntity3);

        categoryVo3 = new CategoryVo();
        categoryVo3.setId(categoryEntity3.getId().toString());
        categoryVo3.setName(categoryEntity3.getName());
        categoryVo3.setDescription(categoryEntity3.getDescription());

        categoryEntity4 = categoryRepository.save(categoryEntity4);

        categoryVo4 = new CategoryVo();
        categoryVo4.setId(categoryEntity4.getId().toString());
        categoryVo4.setName(categoryEntity4.getName());
        categoryVo4.setDescription(categoryEntity4.getDescription());

        /**
         * Item
         */

        itemEntity1 = new ItemEntity();
        itemEntity1.setName("Item 1");
        itemEntity1.setIsVegeterian(Boolean.FALSE);
        itemEntity1.setImageUrl("Item 1 Image");
        itemEntity1.setDescription("Item 1 description");
        itemEntity1.setActive(Boolean.TRUE);
        itemEntity1.setCategory(categoryEntity1);

        itemEntity1 = itemRepository.save(itemEntity1);

        itemVo1 = new ItemVo();
        itemVo1.setId(itemEntity1.getId().toString());
        itemVo1.setName(itemEntity1.getName());
        itemVo1.setIsVegeterian(VegeterianStatus.getSwitchValue(itemEntity1.getIsVegeterian()));
        itemVo1.setCategoryId(itemEntity1.getCategory().getId().toString());
        itemVo1.setDescription(itemEntity1.getDescription());
        itemVo1.setImageUrl(itemEntity1.getImageUrl());

        itemEntity2 = new ItemEntity();
        itemEntity2.setName("Item 2");
        itemEntity2.setIsVegeterian(Boolean.TRUE);
        itemEntity2.setImageUrl("Item 2 Image");
        itemEntity2.setDescription("Item 2 description");
        itemEntity2.setActive(Boolean.TRUE);
        itemEntity2.setCategory(categoryEntity2);

        itemEntity2 = itemRepository.save(itemEntity2);

        itemVo2 = new ItemVo();
        itemVo2.setId(itemEntity2.getId().toString());
        itemVo2.setName(itemEntity2.getName());
        itemVo2.setIsVegeterian(VegeterianStatus.getSwitchValue(itemEntity2.getIsVegeterian()));
        itemVo2.setCategoryId(itemEntity2.getCategory().getId().toString());
        itemVo2.setDescription(itemEntity2.getDescription());
        itemVo2.setImageUrl(itemEntity2.getImageUrl());

        itemEntity3 = new ItemEntity();
        itemEntity3.setName("Item 3");
        itemEntity3.setIsVegeterian(Boolean.FALSE);
        itemEntity3.setImageUrl("Item 3 Image");
        itemEntity3.setDescription("Item 3 description");
        itemEntity3.setActive(Boolean.TRUE);
        itemEntity3.setCategory(categoryEntity3);

        itemEntity3 = itemRepository.save(itemEntity3);

        itemVo3 = new ItemVo();
        itemVo3.setId(itemEntity3.getId().toString());
        itemVo3.setName(itemEntity3.getName());
        itemVo3.setIsVegeterian(VegeterianStatus.getSwitchValue(itemEntity3.getIsVegeterian()));
        itemVo3.setCategoryId(itemEntity3.getCategory().getId().toString());
        itemVo3.setDescription(itemEntity3.getDescription());
        itemVo3.setImageUrl(itemEntity3.getImageUrl());

        itemEntity4 = new ItemEntity();
        itemEntity4.setName("Item 4");
        itemEntity4.setIsVegeterian(Boolean.TRUE);
        itemEntity4.setImageUrl("Item 4 Image");
        itemEntity4.setDescription("Item 4 description");
        itemEntity4.setActive(Boolean.TRUE);
        itemEntity4.setCategory(categoryEntity4);

        itemEntity4 = itemRepository.save(itemEntity4);

        itemVo4 = new ItemVo();
        itemVo4.setId(itemEntity4.getId().toString());
        itemVo4.setName(itemEntity4.getName());
        itemVo4.setIsVegeterian(VegeterianStatus.getSwitchValue(itemEntity4.getIsVegeterian()));
        itemVo4.setCategoryId(itemEntity4.getCategory().getId().toString());
        itemVo4.setDescription(itemEntity4.getDescription());
        itemVo4.setImageUrl(itemEntity4.getImageUrl());

        itemEntity5 = new ItemEntity();
        itemEntity5.setName("Item 5");
        itemEntity5.setIsVegeterian(Boolean.FALSE);
        itemEntity5.setImageUrl("Item 5 Image");
        itemEntity5.setDescription("Item 5 description");
        itemEntity5.setActive(Boolean.TRUE);
        itemEntity5.setCategory(categoryEntity4);

        itemEntity5 = itemRepository.save(itemEntity5);

        itemVo5 = new ItemVo();
        itemVo5.setId(itemEntity5.getId().toString());
        itemVo5.setName(itemEntity5.getName());
        itemVo5.setIsVegeterian(VegeterianStatus.getSwitchValue(itemEntity5.getIsVegeterian()));
        itemVo5.setCategoryId(itemEntity5.getCategory().getId().toString());
        itemVo5.setDescription(itemEntity5.getDescription());
        itemVo5.setImageUrl(itemEntity5.getImageUrl());

        categoryEntity1.setItems(new ArrayList<>(Arrays.asList(itemEntity1)));
        categoryEntity1 = categoryRepository.save(categoryEntity1);
        categoryEntity2.setItems(new ArrayList<>(Arrays.asList(itemEntity2)));
        categoryEntity2 = categoryRepository.save(categoryEntity2);
        categoryEntity3.setItems(new ArrayList<>(Arrays.asList(itemEntity3)));
        categoryEntity3 = categoryRepository.save(categoryEntity3);
        categoryEntity4.setItems(new ArrayList<>(Arrays.asList(itemEntity4, itemEntity5)));
        categoryEntity4 = categoryRepository.save(categoryEntity4);

        itemVo1.setCategory(categoryVo1);
        itemVo2.setCategory(categoryVo2);
        itemVo3.setCategory(categoryVo3);
        itemVo4.setCategory(categoryVo4);
        itemVo5.setCategory(categoryVo4);

        categoryVo1.setItems(Arrays.asList(itemVo1));
        categoryVo2.setItems(Arrays.asList(itemVo2));
        categoryVo3.setItems(Arrays.asList(itemVo3));
        categoryVo4.setItems(Arrays.asList(itemVo4, itemVo5));

    }

    @AfterEach
    private void destroy() {
        itemEntity1.setCategory(null);
        itemEntity2.setCategory(null);
        itemEntity3.setCategory(null);
        itemEntity4.setCategory(null);
        itemEntity5.setCategory(null);

        itemRepository.deleteById(itemEntity1.getId());
        itemRepository.deleteById(itemEntity2.getId());
        itemRepository.deleteById(itemEntity3.getId());
        itemRepository.deleteById(itemEntity4.getId());
        itemRepository.deleteById(itemEntity5.getId());

        categoryEntity1.setItems(null);
        categoryEntity2.setItems(null);
        categoryEntity3.setItems(null);
        categoryEntity4.setItems(null);
        
        categoryRepository.deleteById(categoryEntity1.getId());
        categoryRepository.deleteById(categoryEntity2.getId());
        categoryRepository.deleteById(categoryEntity3.getId());
        categoryRepository.deleteById(categoryEntity4.getId());
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
    public void test_Category_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Post_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenPosted_WithNoCategoryForm() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForAllCategories() throws Exception {
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForCategories_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<CategoryVo> categoryList = new ArrayList<>(Arrays.asList(categoryVo1, categoryVo2, categoryVo3, categoryVo4));

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_FILTER)
                        .queryParam("name", "Category"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(categoryList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo[].class).length);
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForCategories_WithDescription() throws Exception {
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
    public void test_Category_Get_ShouldReturn_200Response_And_CategoryListNaturallyOrdered_WhenRequested_ForCategories_WithNameAndDescription() throws Exception {
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
    public void test_Category_Get_ShouldReturn_200Response_And_EmptyCategoryList_WhenRequested_ForCategories_WithAbsent_WithNameAndDescription() throws Exception {
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
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        categoryVo1.setItems(null);

        mvcResult = this.mockMvc.perform(get(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(categoryVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(categoryVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
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
        String id = categoryEntity3.getId().toString();
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
        String id = categoryEntity1.getId().toString();
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
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getActive()));
    }

    @Test
    public void test_Category_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = categoryEntity1.getId().toString();
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
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getItems() != null);
        Assertions.assertEquals(categoryVo1.getItems().size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getItems().size());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getActive()));
    }

    @Test
    public void test_Category_Delete_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Delete_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Delete_ShouldReturn_400Response_And_ErrorCode_RES_MENU_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = categoryEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Category_Delete_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
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
        String id = categoryEntity1.getId().toString();
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
    @ValueSource(strings = { " ", "r" })
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenUpdatedBy_EmptyInvalidId_AndCategoryDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByAbsentId_AndCategoryDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_005_WhenUpdated_ByInactiveId_AndCategoryDetails() throws Exception {
        String id = categoryEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();

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
    public void test_Category_Put_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndNoCategoryDetails() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndEmptyCategoryDetails() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
        String id = categoryEntity4.getId().toString();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndCategoryDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByInvalidId_AndCategoryDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    }

    @Test
    public void test_Category_Patch_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByAbsentId_AndCategoryDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndNoCategoryDetails() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
        String id = categoryEntity1.getId().toString();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidDefinitionOfCategoryAttribute() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
