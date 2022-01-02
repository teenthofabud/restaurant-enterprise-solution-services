package com.teenthofabud.restaurant.solution.inventory;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryForm;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.product.repository.ProductRepository;
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
public class CategoryIntegrationTest extends InventoryIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String CATEGORY_URI = "/category";
    private static final String CATEGORY_URI_BY_ID = "/category/{id}";
    private static final String CATEGORY_URI_FILTER = "/category/filter";

    private CategoryRepository categoryRepository;
    private ProductRepository productRepository;

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    @Autowired
    public void setProductRepository(ProductRepository productRepository) {
        this.productRepository = productRepository;
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

    private ProductVo productVo1;
    private ProductVo productVo2;
    private ProductVo productVo3;
    private ProductVo productVo4;
    private ProductVo productVo5;
    private ProductEntity productEntity1;
    private ProductEntity productEntity2;
    private ProductEntity productEntity3;
    private ProductEntity productEntity4;
    private ProductEntity productEntity5;

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
         * Product
         */

        productEntity1 = new ProductEntity();
        productEntity1.setName("Product 1");
        productEntity1.setImageUrl("Product 1 Image");
        productEntity1.setDescription("Product 1 description");
        productEntity1.setActive(Boolean.TRUE);
        productEntity1.setCategory(categoryEntity1);

        productEntity1 = productRepository.save(productEntity1);

        productVo1 = new ProductVo();
        productVo1.setId(productEntity1.getId().toString());
        productVo1.setName(productEntity1.getName());
        productVo1.setCategoryId(productEntity1.getCategory().getId().toString());
        productVo1.setDescription(productEntity1.getDescription());
        productVo1.setImageUrl(productEntity1.getImageUrl());

        productEntity2 = new ProductEntity();
        productEntity2.setName("Product 2");
        productEntity2.setImageUrl("Product 2 Image");
        productEntity2.setDescription("Product 2 description");
        productEntity2.setActive(Boolean.TRUE);
        productEntity2.setCategory(categoryEntity2);

        productEntity2 = productRepository.save(productEntity2);

        productVo2 = new ProductVo();
        productVo2.setId(productEntity2.getId().toString());
        productVo2.setName(productEntity2.getName());
        productVo2.setCategoryId(productEntity2.getCategory().getId().toString());
        productVo2.setDescription(productEntity2.getDescription());
        productVo2.setImageUrl(productEntity2.getImageUrl());

        productEntity3 = new ProductEntity();
        productEntity3.setName("Product 3");
        productEntity3.setImageUrl("Product 3 Image");
        productEntity3.setDescription("Product 3 description");
        productEntity3.setActive(Boolean.TRUE);
        productEntity3.setCategory(categoryEntity3);

        productEntity3 = productRepository.save(productEntity3);

        productVo3 = new ProductVo();
        productVo3.setId(productEntity3.getId().toString());
        productVo3.setName(productEntity3.getName());
        productVo3.setCategoryId(productEntity3.getCategory().getId().toString());
        productVo3.setDescription(productEntity3.getDescription());
        productVo3.setImageUrl(productEntity3.getImageUrl());

        productEntity4 = new ProductEntity();
        productEntity4.setName("Product 4");
        productEntity4.setImageUrl("Product 4 Image");
        productEntity4.setDescription("Product 4 description");
        productEntity4.setActive(Boolean.TRUE);
        productEntity4.setCategory(categoryEntity4);

        productEntity4 = productRepository.save(productEntity4);

        productVo4 = new ProductVo();
        productVo4.setId(productEntity4.getId().toString());
        productVo4.setName(productEntity4.getName());
        productVo4.setCategoryId(productEntity4.getCategory().getId().toString());
        productVo4.setDescription(productEntity4.getDescription());
        productVo4.setImageUrl(productEntity4.getImageUrl());

        productEntity5 = new ProductEntity();
        productEntity5.setName("Product 5");
        productEntity5.setImageUrl("Product 5 Image");
        productEntity5.setDescription("Product 5 description");
        productEntity5.setActive(Boolean.TRUE);
        productEntity5.setCategory(categoryEntity4);

        productEntity5 = productRepository.save(productEntity5);

        productVo5 = new ProductVo();
        productVo5.setId(productEntity5.getId().toString());
        productVo5.setName(productEntity5.getName());
        productVo5.setCategoryId(productEntity5.getCategory().getId().toString());
        productVo5.setDescription(productEntity5.getDescription());
        productVo5.setImageUrl(productEntity5.getImageUrl());

        categoryEntity1.setProducts(new ArrayList<ProductEntity>(List.of(productEntity1)));
        categoryEntity1 = categoryRepository.save(categoryEntity1);
        categoryEntity2.setProducts(new ArrayList<ProductEntity>(List.of(productEntity2)));
        categoryEntity2 = categoryRepository.save(categoryEntity2);
        categoryEntity3.setProducts(new ArrayList<ProductEntity>(List.of(productEntity3)));
        categoryEntity3 = categoryRepository.save(categoryEntity3);
        categoryEntity4.setProducts(new ArrayList<ProductEntity>(List.of(productEntity4, productEntity5)));
        categoryEntity4 = categoryRepository.save(categoryEntity4);

        productVo1.setCategory(categoryVo1);
        productVo2.setCategory(categoryVo2);
        productVo3.setCategory(categoryVo3);
        productVo4.setCategory(categoryVo4);
        productVo5.setCategory(categoryVo4);

        categoryVo1.setProducts(Arrays.asList(productVo1));
        categoryVo2.setProducts(Arrays.asList(productVo2));
        categoryVo3.setProducts(Arrays.asList(productVo3));
        categoryVo4.setProducts(Arrays.asList(productVo4, productVo5));

    }

    @AfterEach
    private void destroy() {
        productEntity1.setCategory(null);
        productEntity2.setCategory(null);
        productEntity3.setCategory(null);
        productEntity4.setCategory(null);
        productEntity5.setCategory(null);

        productRepository.deleteById(productEntity1.getId());
        productRepository.deleteById(productEntity2.getId());
        productRepository.deleteById(productEntity3.getId());
        productRepository.deleteById(productEntity4.getId());
        productRepository.deleteById(productEntity5.getId());

        categoryEntity1.setProducts(null);
        categoryEntity2.setProducts(null);
        categoryEntity3.setProducts(null);
        categoryEntity4.setProducts(null);
        
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
    public void test_Category_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Post_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenPosted_WithNoCategoryForm() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
        categoryVo1.setProducts(null);

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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenRequested_ByAbsentId() throws Exception {
        String id = String.valueOf(Long.MAX_VALUE);
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
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
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getActive()));
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
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getProducts() != null);
        Assertions.assertEquals(categoryVo1.getProducts().size(), om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getProducts().size());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CategoryVo.class).getActive()));
    }

    @Test
    public void test_Category_Delete_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Delete_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Delete_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = categoryEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(CATEGORY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Category_Delete_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenUpdatedBy_EmptyInvalidId_AndCategoryDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByAbsentId_AndCategoryDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_005_WhenUpdated_ByInactiveId_AndCategoryDetails() throws Exception {
        String id = categoryEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();

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
    public void test_Category_Put_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndNoCategoryDetails() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Put_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndEmptyCategoryDetails() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByInvalidId_AndCategoryDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByAbsentId_AndCategoryDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndNoCategoryDetails() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Category_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidDefinitionOfCategoryAttribute() throws Exception {
        String id = categoryEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
