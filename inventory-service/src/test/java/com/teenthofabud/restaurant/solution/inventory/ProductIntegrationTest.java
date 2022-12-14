package com.teenthofabud.restaurant.solution.inventory;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryEntity;
import com.teenthofabud.restaurant.solution.inventory.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.inventory.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.inventory.error.InventoryErrorCode;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductEntity;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductForm;
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
public class ProductIntegrationTest extends InventoryIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ITEM_URI = "/product";
    private static final String ITEM_URI_BY_ID = "/product/{id}";
    private static final String ITEM_URI_BY_CATEGORY_ID = "/product/categoryid/{categoryId}";
    private static final String ITEM_URI_FILTER = "/product/filter";

    private ProductRepository productRepository;
    private CategoryRepository categoryRepository;

    @Autowired
    public void setProductRepository(ProductRepository productRepository) {
        this.productRepository = productRepository;
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

    private ProductForm productForm;
    private ProductVo productVo1;
    private ProductVo productVo2;
    private ProductVo productVo3;
    private ProductVo productVo4;
    private ProductVo productVo5;
    private ProductVo productVo6;
    private ProductEntity productEntity1;
    private ProductEntity productEntity2;
    private ProductEntity productEntity3;
    private ProductEntity productEntity4;
    private ProductEntity productEntity5;

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
         * Product
         */

        productForm = new ProductForm();
        productForm.setDescription("Product New Description");
        productForm.setName("Product New Name");
        productForm.setCategoryId(categoryEntity3.getId().toString());
        productForm.setImageUrl("Product New Image");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched first name"),
                new PatchOperationForm("replace", "/categoryId", categoryEntity3.getId().toString()));

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
        productVo1.setCategory(categoryVo1);

        productEntity2 = new ProductEntity();
        productEntity2.setName("Product 2");
        productEntity2.setImageUrl("Product 2 Image");
        productEntity2.setDescription("Product 2 description");
        productEntity2.setActive(Boolean.FALSE);
        productEntity2.setCategory(categoryEntity2);

        productEntity2 = productRepository.save(productEntity2);

        productVo2 = new ProductVo();
        productVo2.setId(productEntity2.getId().toString());
        productVo2.setName(productEntity2.getName());
        productVo2.setCategoryId(productEntity2.getCategory().getId().toString());
        productVo2.setDescription(productEntity2.getDescription());
        productVo2.setImageUrl(productEntity2.getImageUrl());
        productVo2.setCategory(categoryVo2);

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
        productVo3.setCategory(categoryVo3);

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
        productVo4.setCategory(categoryVo4);

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
        productVo5.setCategory(categoryVo4);

        productVo6 = new ProductVo();
        productVo6.setId(UUID.randomUUID().toString());
        productVo6.setName(productForm.getName());
        productVo6.setCategoryId(productForm.getCategoryId());
        productVo6.setDescription(productForm.getDescription());
        productVo6.setImageUrl(productForm.getImageUrl());
        productVo6.setCategory(categoryVo3);

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

        categoryRepository.deleteById(categoryEntity1.getId());
        categoryRepository.deleteById(categoryEntity2.getId());
        categoryRepository.deleteById(categoryEntity3.getId());
        categoryRepository.deleteById(categoryEntity4.getId());
    }

    @Test
    public void test_Product_Post_ShouldReturn_201Response_And_NewProductId_WhenPosted_WithValidProductForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Product_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        productForm.setName("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }
    
    @Test
    public void test_Product_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithEmptyImageUrl() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "imageUrl";
        productForm.setImageUrl("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Product_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithEmptyCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        productForm.setCategoryId("");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Product_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithInactiveCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        productForm.setCategoryId(categoryEntity2.getId().toString());

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Product_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithInvalidCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        productForm.setCategoryId("r");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Product_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithAbsentCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        productForm.setCategoryId("99999");

        mvcResult = mockMvc.perform(post(ITEM_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Product_Post_ShouldReturn_409Response_And_ErrorCode_RES_INVENTORY_004_WhenRequested_WithDuplicateProduct() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "categoryId";
        String field1Value = productEntity1.getName();
        String field2Value = productEntity1.getCategory().getId().toString();
        productForm.setName(field1Value);
        productForm.setCategoryId(field2Value);

        mvcResult = mockMvc.perform(post(ITEM_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
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
    public void test_Product_Post_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenPosted_WithNoProductForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_200Response_And_ProductListNaturallyOrdered_WhenRequested_ForAllProducts() throws Exception {
        MvcResult mvcResult = null;
        List<ProductVo> productList = new ArrayList<>(Arrays.asList(productVo5, productVo4, productVo1));

        mvcResult = this.mockMvc.perform(get(ITEM_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo[].class).length);
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_ProductListNaturallyOrdered_WhenRequested_ForProducts_ByCategoryId() throws Exception {
        MvcResult mvcResult = null;
        productVo4.setCategory(null);
        productVo5.setCategory(null);
        List<ProductVo> productList = Arrays.asList(productVo4, productVo5);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_CATEGORY_ID, categoryEntity4.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(productList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_ProductListNaturallyOrdered_WhenRequested_ForProducts_ByEmptyCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ByAbsentCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String categoryId = "kk";
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ByInactiveCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String categoryId = categoryEntity2.getId().toString();
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyImageUrlOnly(String imageUrl) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyIsVegeterianOnly(String isVegeterian) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyCategoryIdOnly(String categoryId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_200Response_And_EmptyProductList_WhenRequestedBy_AbsentNameName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo[].class).length);
    }

    @Test
    public void test_Product_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001__WhenRequestedBy_UnsupportedFilterAttribute() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_200Response_And_ProductListNaturallyOrdered_WhenRequested_ForProducts_WithName() throws Exception {
        MvcResult mvcResult = null;
        productVo1.setCategory(null);
        List<ProductVo> productList = new ArrayList<>(Arrays.asList(productVo1));

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("name", "Product 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(productList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_ProductListNaturallyOrdered_WhenRequested_ForProducts_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        productVo1.setCategory(null);
        List<ProductVo> productList = new ArrayList<>(Arrays.asList(productVo1));

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("description", "Product 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(productList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_ProductListNaturallyOrdered_WhenRequested_ForProducts_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        productVo3.setCategory(null);
        productVo2.setCategory(null);
        productVo4.setCategory(null);
        productVo1.setCategory(null);
        productVo5.setCategory(null);
        List<ProductVo> productList = Arrays.asList(productVo1, productVo2, productVo3, productVo4, productVo5);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("name", "Product")
                        .queryParam("description", "Product"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(productList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_EmptyProductList_WhenRequested_ForProducts_WithAbsent_NameAndIsVegeterianAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<ProductVo> productList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(ITEM_URI_FILTER)
                        .queryParam("name", "Product 1")
                        .queryParam("isVegeterian", "NO")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo[].class).length);
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_ProductDetails_WhenRequested_ById() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        productVo1.setCategory(null);

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(productVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(productVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Product_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
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
    public void test_Product_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = productEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getId());
        Assertions.assertEquals(productVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getName());
        Assertions.assertEquals(productVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getDescription());
        Assertions.assertEquals(productVo1.getImageUrl(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getImageUrl());
        Assertions.assertEquals(productVo1.getCategoryId(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getCategoryId());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getActive()));
    }

    @Test
    public void test_Product_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ITEM_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(productVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getId());
        Assertions.assertEquals(productVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getName());
        Assertions.assertEquals(productVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getDescription());
        Assertions.assertEquals(productVo1.getImageUrl(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getImageUrl());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getCategory() != null);
        Assertions.assertEquals(productVo1.getCategory().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getCategory().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ProductVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Product_Delete_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Delete_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = productEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ITEM_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Product_Delete_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
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
    public void test_Product_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndProductDetails() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        productForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Product_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenUpdatedBy_EmptyInvalidId_AndProductDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Product_Put_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByAbsentId_AndProductDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Product_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_005_WhenUpdated_ByInactiveId_AndProductDetails() throws Exception {
        String id = productEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Product_Put_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndNoProductDetails() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Product_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndEmptyName(String name) throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        productForm.setName(name);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Product_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndEmptyInvalidCategoryId(String categoryId) throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        productForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999" })
    public void test_Product_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndAbsentCategoryId(String categoryId) throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        productForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Product_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInactiveCategoryId() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String categoryId = categoryEntity2.getId().toString();
        String fieldName = "categoryId";
        productForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(productForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Product_Put_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndEmptyProductDetails() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new ProductForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Product_Put_ShouldReturn_409Response_And_ErrorCode_RES_INVENTORY_004_WhenUpdated_ById_AndDuplicateProductDetails() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "categoryId";
        String field1Value = productEntity1.getName();
        String field2Value = productEntity1.getCategory().getId().toString();
        productForm.setName(field1Value);
        productForm.setCategoryId(field2Value);

        mvcResult = mockMvc.perform(put(ITEM_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(productForm)))
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
    public void test_Product_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndProductDetails() throws Exception {
        String id = productEntity1.getId().toString();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndProductDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByInvalidId_AndProductDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Patch_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByAbsentId_AndProductDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
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
    public void test_Product_Patch_ShouldReturn_409Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ById_AndDuplicateProductDetails() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "categoryId";
        String field1Value = productEntity1.getName();
        String field2Value = productEntity1.getCategory().getId().toString();
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
    public void test_Product_Patch_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndNoProductDetails() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = productEntity1.getId().toString();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyIsVegeterian(String isVegeterian) throws Exception {
        String id = productEntity1.getId().toString();
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
    @ValueSource(strings = { " ", "" })
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCategoryId(String categoryId) throws Exception {
        String id = productEntity1.getId().toString();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidCategoryId(String categoryId) throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveCategoryId() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
    public void test_Product_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidDefinitionOfProductAttribute() throws Exception {
        String id = productEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
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
