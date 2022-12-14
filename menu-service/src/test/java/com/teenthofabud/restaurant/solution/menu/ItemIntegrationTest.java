package com.teenthofabud.restaurant.solution.menu;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemForm;
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class ItemIntegrationTest extends MenuIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ITEM_URI = "/item";
    private static final String ITEM_URI_BY_ID = "/item/{id}";
    private static final String ITEM_URI_BY_CATEGORY_ID = "/item/categoryid/{categoryId}";
    private static final String ITEM_URI_FILTER = "/item/filter";

    private ItemRepository itemRepository;
    private CategoryRepository categoryRepository;

    @Autowired
    public void setItemRepository(ItemRepository itemRepository) {
        this.itemRepository = itemRepository;
    }

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    private CategoryVo categoryVo1;
    private CategoryVo categoryVo2;
    private CategoryVo categoryVo3;
    private CategoryVo categoryVo4;
    private CategoryEntity categoryEntity1;
    private CategoryEntity categoryEntity2;
    private CategoryEntity categoryEntity3;
    private CategoryEntity categoryEntity4;

    private ItemForm itemForm;
    private ItemVo itemVo1;
    private ItemVo itemVo2;
    private ItemVo itemVo3;
    private ItemVo itemVo4;
    private ItemVo itemVo5;
    private ItemVo itemVo6;
    private ItemEntity itemEntity1;
    private ItemEntity itemEntity2;
    private ItemEntity itemEntity3;
    private ItemEntity itemEntity4;
    private ItemEntity itemEntity5;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Category
         */

        categoryEntity1 = new CategoryEntity();
        categoryEntity1.setName("Category 1 Name");
        categoryEntity1.setDescription("Category 1 Description");
        categoryEntity1.setActive(Boolean.TRUE);

        categoryEntity1 = categoryRepository.save(categoryEntity1);

        categoryVo1 = new CategoryVo();
        categoryVo1.setId(categoryEntity1.getId().toString());
        categoryVo1.setName(categoryEntity1.getName());
        categoryVo1.setDescription(categoryEntity1.getDescription());

        categoryEntity2 = new CategoryEntity();
        categoryEntity2.setName("Category 2 Name");
        categoryEntity2.setDescription("Category 2 Description");
        categoryEntity2.setActive(Boolean.FALSE);

        categoryEntity2 = categoryRepository.save(categoryEntity2);

        categoryVo2 = new CategoryVo();
        categoryVo2.setId(categoryEntity2.getId().toString());
        categoryVo2.setName(categoryEntity2.getName());
        categoryVo2.setDescription(categoryEntity2.getDescription());

        categoryEntity3 = new CategoryEntity();
        categoryEntity3.setName("Category 3 Name");
        categoryEntity3.setDescription("Category 3 Description");
        categoryEntity3.setActive(Boolean.TRUE);

        categoryEntity3 = categoryRepository.save(categoryEntity3);

        categoryVo3 = new CategoryVo();
        categoryVo3.setId(categoryEntity3.getId().toString());
        categoryVo3.setName(categoryEntity3.getName());
        categoryVo3.setDescription(categoryEntity3.getDescription());

        categoryEntity4 = new CategoryEntity();
        categoryEntity4.setName("Category 4 Name");
        categoryEntity4.setDescription("Category 4 Description");
        categoryEntity4.setActive(Boolean.TRUE);

        categoryEntity4 = categoryRepository.save(categoryEntity4);

        categoryVo4 = new CategoryVo();
        categoryVo4.setId(categoryEntity4.getId().toString());
        categoryVo4.setName(categoryEntity4.getName());
        categoryVo4.setDescription(categoryEntity4.getDescription());

        /**
         * Item
         */

        itemForm = new ItemForm();
        itemForm.setDescription("Item New Description");
        itemForm.setName("Item New Name");
        itemForm.setCategoryId(categoryEntity3.getId().toString());
        itemForm.setImageUrl("Item New Image");
        itemForm.setIsVegeterian("YES");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched first name"),
                new PatchOperationForm("replace", "/categoryId", categoryEntity3.getId().toString()),
                new PatchOperationForm("replace", "/isVegeterian", "NO"));

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
        itemVo1.setCategory(categoryVo1);

        itemEntity2 = new ItemEntity();
        itemEntity2.setName("Item 2");
        itemEntity2.setIsVegeterian(Boolean.TRUE);
        itemEntity2.setImageUrl("Item 2 Image");
        itemEntity2.setDescription("Item 2 description");
        itemEntity2.setActive(Boolean.FALSE);
        itemEntity2.setCategory(categoryEntity2);

        itemEntity2 = itemRepository.save(itemEntity2);

        itemVo2 = new ItemVo();
        itemVo2.setId(itemEntity2.getId().toString());
        itemVo2.setName(itemEntity2.getName());
        itemVo2.setIsVegeterian(VegeterianStatus.getSwitchValue(itemEntity2.getIsVegeterian()));
        itemVo2.setCategoryId(itemEntity2.getCategory().getId().toString());
        itemVo2.setDescription(itemEntity2.getDescription());
        itemVo2.setImageUrl(itemEntity2.getImageUrl());
        itemVo2.setCategory(categoryVo2);

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
        itemVo3.setCategory(categoryVo3);

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
        itemVo4.setCategory(categoryVo4);

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
        itemVo5.setCategory(categoryVo4);

        itemVo6 = new ItemVo();
        itemVo6.setId(UUID.randomUUID().toString());
        itemVo6.setName(itemForm.getName());
        itemVo6.setIsVegeterian(VegeterianStatus.valueOf(itemForm.getIsVegeterian()));
        itemVo6.setCategoryId(itemForm.getCategoryId());
        itemVo6.setDescription(itemForm.getDescription());
        itemVo6.setImageUrl(itemForm.getImageUrl());
        itemVo6.setCategory(categoryVo3);

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

        categoryRepository.deleteById(categoryEntity1.getId());
        categoryRepository.deleteById(categoryEntity2.getId());
        categoryRepository.deleteById(categoryEntity3.getId());
        categoryRepository.deleteById(categoryEntity4.getId());
    }

    @Test
    public void test_Item_Post_ShouldReturn_201Response_And_NewItemId_WhenPosted_WithValidItemForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        itemForm.setName("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyIsVegeterian() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "isVegeterian";
        itemForm.setIsVegeterian("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }
    
    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyImageUrl() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "imageUrl";
        itemForm.setImageUrl("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        itemForm.setCategoryId("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInactiveCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        itemForm.setCategoryId(categoryEntity2.getId().toString());

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInvalidIsVegeterian() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "isVegeterian";
        itemForm.setIsVegeterian("r");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInvalidCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        itemForm.setCategoryId("r");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Item_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithAbsentCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        itemForm.setCategoryId("99999");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Item_Post_ShouldReturn_409Response_And_ErrorCode_RES_MENU_004_WhenRequested_WithDuplicateItem() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "categoryId";
        String field1Value = itemEntity1.getName();
        String field2Value = itemEntity1.getCategory().getId().toString();
        itemForm.setName(field1Value);
        itemForm.setCategoryId(field2Value);

        mvcResult = mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
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
    public void test_Item_Post_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenPosted_WithNoItemForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForAllItems() throws Exception {
        MvcResult mvcResult = null;
        List<ItemVo> itemList = new ArrayList<>(Arrays.asList(itemVo5, itemVo1));

        mvcResult = this.mockMvc.perform(get(ITEM_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_ByCategoryId() throws Exception {
        MvcResult mvcResult = null;
        itemVo4.setCategory(null);
        itemVo5.setCategory(null);
        List<ItemVo> itemList = Arrays.asList(itemVo4, itemVo5);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_CATEGORY_ID, categoryEntity4.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(itemList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_ByEmptyCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_CATEGORY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Item_Get_ShouldReturn_404Response_And_ErrorCode_RES_MENU_001_WhenRequested_ByAbsentCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String categoryId = "kk";
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_CATEGORY_ID, categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(categoryId));
    }

    @Test
    public void test_Item_Get_ShouldReturn_404Response_And_ErrorCode_RES_MENU_001_WhenRequested_ByInactiveCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String categoryId = categoryEntity2.getId().toString();
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();
        String errorName = "deactivated";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_CATEGORY_ID, categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(errorName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(categoryId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyImageUrlOnly(String imageUrl) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("imageUrl", imageUrl))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyIsVegeterianOnly(String isVegeterian) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("isVegeterian", isVegeterian))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyCategoryIdOnly(String categoryId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("categoryId", categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_EmptyItemList_WhenRequestedBy_AbsentNameName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
    }

    @Test
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001__WhenRequestedBy_UnsupportedFilterAttribute() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("imageUrl", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_InvalidIsVegeterian() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "isVegeterian";

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("isVegeterian", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_WithName() throws Exception {
        MvcResult mvcResult = null;
        itemVo1.setCategory(null);
        List<ItemVo> itemList = new ArrayList<>(Arrays.asList(itemVo1));

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("name", "Item 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(itemList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        itemVo1.setCategory(null);
        List<ItemVo> itemList = new ArrayList<>(Arrays.asList(itemVo1));

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("description", "Item 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(itemList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_WithIsVegeterian() throws Exception {
        MvcResult mvcResult = null;
        itemVo2.setCategory(null);
        itemVo4.setCategory(null);
        List<ItemVo> itemList = new ArrayList<>(Arrays.asList(itemVo2, itemVo4));

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("isVegeterian", "YES"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(itemList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_WithNameAndIsVegeterian() throws Exception {
        MvcResult mvcResult = null;
        itemVo2.setCategory(null);
        itemVo4.setCategory(null);
        List<ItemVo> itemList = Arrays.asList(itemVo2, itemVo4);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("isVegeterian", "YES")
                        .queryParam("name", "Item"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(itemList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_WithDescriptionAndIsVegeterian() throws Exception {
        MvcResult mvcResult = null;
        itemVo1.setCategory(null);
        itemVo3.setCategory(null);
        itemVo5.setCategory(null);
        List<ItemVo> itemList = Arrays.asList(itemVo1, itemVo3, itemVo5);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("description", "Item")
                        .queryParam("isVegeterian", "NO"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(itemList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemListNaturallyOrdered_WhenRequested_ForItems_WithNameAndIsVegeterianAndDescription() throws Exception {
        MvcResult mvcResult = null;
        itemVo1.setCategory(null);
        itemVo3.setCategory(null);
        itemVo5.setCategory(null);
        List<ItemVo> itemList = Arrays.asList(itemVo1, itemVo3, itemVo5);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("name", "Item")
                        .queryParam("isVegeterian", "NO")
                        .queryParam("description", "Item"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(itemList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_EmptyItemList_WhenRequested_ForItems_WithAbsent_NameAndIsVegeterianAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<ItemVo> itemList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("name", "Item 1")
                        .queryParam("isVegeterian", "NO")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo[].class).length);
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_ItemDetails_WhenRequested_ById() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        itemVo1.setCategory(null);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(itemVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(itemVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Item_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
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
    public void test_Item_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = itemEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getId());
        Assertions.assertEquals(itemVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getName());
        Assertions.assertEquals(itemVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getDescription());
        Assertions.assertEquals(itemVo1.getImageUrl(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getImageUrl());
        Assertions.assertEquals(itemVo1.getIsVegeterian(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getIsVegeterian());
        Assertions.assertEquals(itemVo1.getCategoryId(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getCategoryId());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getActive()));
    }

    @Test
    public void test_Item_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(itemVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getId());
        Assertions.assertEquals(itemVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getName());
        Assertions.assertEquals(itemVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getDescription());
        Assertions.assertEquals(itemVo1.getImageUrl(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getImageUrl());
        Assertions.assertEquals(itemVo1.getIsVegeterian(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getIsVegeterian());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getCategory() != null);
        Assertions.assertEquals(itemVo1.getCategory().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getCategory().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ItemVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Item_Delete_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Item_Delete_ShouldReturn_400Response_And_ErrorCode_RES_MENU_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = itemEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Item_Delete_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
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
    public void test_Item_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndItemDetails() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        itemForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Item_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenUpdatedBy_EmptyInvalidId_AndItemDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Item_Put_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByAbsentId_AndItemDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Item_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_005_WhenUpdated_ByInactiveId_AndItemDetails() throws Exception {
        String id = itemEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Item_Put_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndNoItemDetails() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
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

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Item_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndEmptyName(String name) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        itemForm.setName(name);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Item_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndEmptyInvalidIsVegeterian(String isVegeterian) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "isVegeterian";
        itemForm.setIsVegeterian(isVegeterian);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Item_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndEmptyInvalidCategoryId(String categoryId) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        itemForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999" })
    public void test_Item_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndAbsentCategoryId(String categoryId) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        itemForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Item_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInactiveCategoryId() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String categoryId = categoryEntity2.getId().toString();
        String fieldName = "categoryId";
        itemForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(itemForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Item_Put_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndEmptyItemDetails() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new ItemForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Item_Put_ShouldReturn_409Response_And_ErrorCode_RES_MENU_004_WhenUpdated_ById_AndDuplicateItemDetails() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "categoryId";
        String field1Value = itemEntity1.getName();
        String field2Value = itemEntity1.getCategory().getId().toString();
        itemForm.setName(field1Value);
        itemForm.setCategoryId(field2Value);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(itemForm)))
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
    public void test_Item_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndItemDetails() throws Exception {
        String id = itemEntity1.getId().toString();
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
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndItemDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByInvalidId_AndItemDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ITEM_URI_BY_ID, id)
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
    public void test_Item_Patch_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByAbsentId_AndItemDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
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
    public void test_Item_Patch_ShouldReturn_409Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ById_AndDuplicateItemDetails() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "categoryId";
        String field1Value = itemEntity1.getName();
        String field2Value = itemEntity1.getCategory().getId().toString();
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
    public void test_Item_Patch_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndNoItemDetails() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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

    @Test
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

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
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyIsVegeterian(String isVegeterian) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/isVegeterian", isVegeterian));

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
    @ValueSource(strings = { "r" })
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidIsVegeterian(String isVegeterian) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "isVegeterian";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, isVegeterian));

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
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCategoryId(String categoryId) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/categoryId", categoryId));

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
    @ValueSource(strings = { "r" })
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidCategoryId(String categoryId) throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, categoryId));

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
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveCategoryId() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String categoryId = categoryEntity2.getId().toString();
        String fieldName = "categoryId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, categoryId));

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
    public void test_Item_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidDefinitionOfItemAttribute() throws Exception {
        String id = itemEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
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

}
