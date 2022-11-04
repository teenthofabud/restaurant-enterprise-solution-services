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
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import com.teenthofabud.restaurant.solution.inventory.product.repository.ProductRepository;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityEntity;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityForm;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.QuantityVo;
import com.teenthofabud.restaurant.solution.inventory.quantity.data.UnitVo;
import com.teenthofabud.restaurant.solution.inventory.quantity.repository.QuantityRepository;
import com.teenthofabud.restaurant.solution.inventory.utils.InventoryServiceHelper;
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
import tech.units.indriya.unit.Units;

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
public class QuantityIntegrationTest extends InventoryIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String QUANTITY_URI = "/quantity";
    private static final String QUANTITY_URI_BY_ID = "/quantity/{id}";
    private static final String QUANTITY_URI_BY_PRODUCT_ID = "/quantity/productid/{productId}";
    private static final String QUANTITY_URI_FILTER = "/quantity/filter";

    private QuantityRepository quantityRepository;
    private ProductRepository productRepository;
    private CategoryRepository categoryRepository;
    private InventoryServiceHelper inventoryServiceHelper;

    @Autowired
    public void setInventoryServiceHelper(InventoryServiceHelper inventoryServiceHelper) {
        this.inventoryServiceHelper = inventoryServiceHelper;
    }

    @Autowired
    public void setQuantityRepository(QuantityRepository quantityRepository) {
        this.quantityRepository = quantityRepository;
    }

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

    private ProductVo productVo1;
    private ProductVo productVo2;
    private ProductVo productVo3;
    private ProductVo productVo4;
    private ProductEntity productEntity1;
    private ProductEntity productEntity2;
    private ProductEntity productEntity3;
    private ProductEntity productEntity4;

    private QuantityForm quantityForm;
    private QuantityVo quantityVo1;
    private QuantityVo quantityVo2;
    private QuantityVo quantityVo3;
    private QuantityVo quantityVo4;
    private QuantityVo quantityVo5;
    private QuantityVo quantityVo6;
    private QuantityEntity quantityEntity1;
    private QuantityEntity quantityEntity2;
    private QuantityEntity quantityEntity3;
    private QuantityEntity quantityEntity4;
    private QuantityEntity quantityEntity5;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

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

        quantityForm = new QuantityForm();
        quantityForm.setAmount(12.0);
        quantityForm.setWeightId(Units.KILOGRAM.getSymbol());
        quantityForm.setProductId(productEntity3.getId().toString());

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/amount", "0.13"),
                new PatchOperationForm("replace", "/weightId", Units.KILOGRAM.getSymbol()),
                new PatchOperationForm("replace", "/productId", productEntity3.getId().toString()));

        quantityEntity1 = new QuantityEntity();
        quantityEntity1.setAmount(22.0d);
        quantityEntity1.setWeightId(Units.KILOGRAM.getSymbol());
        quantityEntity1.setActive(Boolean.TRUE);
        quantityEntity1.setProduct(productEntity1);

        quantityEntity1 = quantityRepository.save(quantityEntity1);

        quantityVo1 = new QuantityVo();
        quantityVo1.setId(quantityEntity1.getId().toString());
        quantityVo1.setAmount(quantityEntity1.getAmount());
        quantityVo1.setWeight(new UnitVo(inventoryServiceHelper.parseWeightCode(quantityEntity1.getWeightId()).get().getName()));
        quantityVo1.setWeightId(quantityEntity1.getWeightId());
        quantityVo1.setProductId(quantityEntity1.getProduct().getId().toString());
        quantityVo1.setProduct(productVo1);

        quantityEntity2 = new QuantityEntity();
        quantityEntity2.setAmount(32.0d);
        quantityEntity2.setWeightId(Units.KILOGRAM.getSymbol());
        quantityEntity2.setActive(Boolean.FALSE);
        quantityEntity2.setProduct(productEntity2);

        quantityEntity2 = quantityRepository.save(quantityEntity2);

        quantityVo2 = new QuantityVo();
        quantityVo2.setId(quantityEntity2.getId().toString());
        quantityVo2.setAmount(quantityEntity2.getAmount());
        quantityVo2.setWeight(new UnitVo(inventoryServiceHelper.parseWeightCode(quantityEntity2.getWeightId()).get().getName()));
        quantityVo2.setWeightId(quantityEntity2.getWeightId());
        quantityVo2.setProductId(quantityEntity2.getProduct().getId().toString());
        quantityVo2.setProduct(productVo2);

        quantityEntity3 = new QuantityEntity();
        quantityEntity3.setAmount(0.0042);
        quantityEntity3.setWeightId(Units.KILOGRAM.getSymbol());
        quantityEntity3.setActive(Boolean.TRUE);
        quantityEntity3.setProduct(productEntity3);

        quantityEntity3 = quantityRepository.save(quantityEntity3);

        quantityVo3 = new QuantityVo();
        quantityVo3.setId(quantityEntity3.getId().toString());
        quantityVo3.setAmount(quantityEntity3.getAmount());
        quantityVo3.setWeight(new UnitVo(inventoryServiceHelper.parseWeightCode(quantityEntity3.getWeightId()).get().getName()));
        quantityVo3.setWeightId(quantityEntity3.getWeightId());
        quantityVo3.setProductId(quantityEntity3.getProduct().getId().toString());
        quantityVo3.setProduct(productVo3);

        quantityEntity4 = new QuantityEntity();
        quantityEntity4.setAmount(0.052);
        quantityEntity4.setWeightId(Units.KILOGRAM.getSymbol());
        quantityEntity4.setActive(Boolean.TRUE);
        quantityEntity4.setProduct(productEntity4);

        quantityEntity4 = quantityRepository.save(quantityEntity4);

        quantityVo4 = new QuantityVo();
        quantityVo4.setId(quantityEntity4.getId().toString());
        quantityVo4.setAmount(quantityEntity4.getAmount());
        quantityVo4.setWeight(new UnitVo(inventoryServiceHelper.parseWeightCode(quantityEntity4.getWeightId()).get().getName()));
        quantityVo4.setWeightId(quantityEntity4.getWeightId());
        quantityVo4.setProductId(quantityEntity4.getProduct().getId().toString());
        quantityVo4.setProduct(productVo4);

        quantityEntity5 = new QuantityEntity();
        quantityEntity5.setAmount(442.0d);
        quantityEntity5.setWeightId(Units.KILOGRAM.getSymbol());
        quantityEntity5.setActive(Boolean.TRUE);
        quantityEntity5.setProduct(productEntity4);

        quantityEntity5 = quantityRepository.save(quantityEntity5);

        quantityVo5 = new QuantityVo();
        quantityVo5.setId(quantityEntity5.getId().toString());
        quantityVo5.setAmount(quantityEntity5.getAmount());
        quantityVo5.setWeight(new UnitVo(inventoryServiceHelper.parseWeightCode(quantityEntity5.getWeightId()).get().getName()));
        quantityVo5.setWeightId(quantityEntity5.getWeightId());
        quantityVo5.setProductId(quantityEntity5.getProduct().getId().toString());
        quantityVo5.setProduct(productVo4);

        quantityVo6 = new QuantityVo();
        quantityVo6.setId(UUID.randomUUID().toString());
        quantityVo6.setAmount(quantityForm.getAmount());
        quantityVo6.setWeight(new UnitVo(inventoryServiceHelper.parseWeightCode(quantityForm.getWeightId()).get().getName()));
        quantityVo6.setWeightId(quantityForm.getWeightId());
        quantityVo6.setProductId(quantityForm.getProductId());
        quantityVo6.setProduct(productVo3);

    }

    @AfterEach
    private void destroy() {
        quantityEntity1.setProduct(null);
        quantityEntity2.setProduct(null);
        quantityEntity3.setProduct(null);
        quantityEntity4.setProduct(null);
        quantityEntity5.setProduct(null);

        quantityRepository.deleteById(quantityEntity1.getId());
        quantityRepository.deleteById(quantityEntity2.getId());
        quantityRepository.deleteById(quantityEntity3.getId());
        quantityRepository.deleteById(quantityEntity4.getId());
        quantityRepository.deleteById(quantityEntity5.getId());

        productRepository.deleteById(productEntity1.getId());
        productRepository.deleteById(productEntity2.getId());
        productRepository.deleteById(productEntity3.getId());
        productRepository.deleteById(productEntity4.getId());
    }

    @Test
    public void test_Quantity_Post_ShouldReturn_201Response_And_NewQuantityId_WhenPosted_WithValidQuantityForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(QUANTITY_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithEmptyAmount() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "amount";
        quantityForm.setAmount(null);

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithEmptyWeightId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "weightId";
        quantityForm.setWeightId("");

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithEmptyProductId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        quantityForm.setProductId("");

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithInactiveProductId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        quantityForm.setProductId(productEntity2.getId().toString());

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithInvalidAmount() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "amount";
        quantityForm.setAmount(-9.0d);

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithInvalidWeightId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "weightId";
        quantityForm.setWeightId("r");

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithInvalidProductId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        quantityForm.setProductId("r");

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithAbsentProductId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        quantityForm.setProductId(String.valueOf(Long.MAX_VALUE));

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Quantity_Post_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenPosted_WithNoQuantityForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(QUANTITY_URI)
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
    public void test_Quantity_Get_ShouldReturn_200Response_And_QuantityListNaturallyOrdered_WhenRequested_ForAllQuantities() throws Exception {
        MvcResult mvcResult = null;
        List<QuantityVo> quantityList = new ArrayList<>(Arrays.asList(quantityVo1, quantityVo2, quantityVo3, quantityVo4));

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(quantityList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo[].class).length);
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_200Response_And_QuantityListNaturallyOrdered_WhenRequested_ForQuantities_ByProductId() throws Exception {
        MvcResult mvcResult = null;
        quantityVo4.setProduct(null);
        quantityVo4.setWeight(null);
        quantityVo5.setProduct(null);
        quantityVo5.setWeight(null);
        List<QuantityVo> quantityList = Arrays.asList(quantityVo4, quantityVo5);

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_PRODUCT_ID, productEntity4.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(quantityList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(quantityList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_200Response_And_QuantityListNaturallyOrdered_WhenRequested_ForQuantities_ByEmptyProductId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_PRODUCT_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ByAbsentProductId() throws Exception {
        MvcResult mvcResult = null;
        String productId = String.valueOf(Long.MAX_VALUE);
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_PRODUCT_ID, productId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(productId));
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ByInvalidProductId() throws Exception {
        MvcResult mvcResult = null;
        String productId = "kk";
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_PRODUCT_ID, productId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(productId));
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ByInactiveProductId() throws Exception {
        MvcResult mvcResult = null;
        String productId = productEntity2.getId().toString();
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();
        String errorName = "deactivated";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_PRODUCT_ID, productId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(errorName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(productId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Quantity_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyWeightIdOnly(String weightId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_FILTER).queryParam("weightId", weightId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Quantity_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyProductIdOnly(String productId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_FILTER).queryParam("productId", productId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }


    @Test
    public void test_Quantity_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001__WhenRequestedBy_UnsupportedFilterAttribute() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_FILTER).queryParam("imageUrl", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_InvalidWeightId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "weightId";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_FILTER).queryParam("weightId", "r1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_200Response_And_QuantityListNaturallyOrdered_WhenRequested_ForQuantities_WithWeightId() throws Exception {
        MvcResult mvcResult = null;
        quantityVo1.setProduct(null);
        quantityVo1.setWeight(null);
        quantityVo2.setProduct(null);
        quantityVo2.setWeight(null);
        quantityVo3.setProduct(null);
        quantityVo3.setWeight(null);
        quantityVo4.setProduct(null);
        quantityVo4.setWeight(null);
        quantityVo5.setProduct(null);
        quantityVo5.setWeight(null);

        List<QuantityVo> quantityList = new ArrayList<>(Arrays.asList(quantityVo1, quantityVo2, quantityVo3, quantityVo4, quantityVo5));

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_FILTER)
                        .queryParam("weightId", Units.KILOGRAM.getSymbol()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(quantityList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(quantityList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_200Response_And_QuantityDetails_WhenRequested_ById() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        quantityVo1.setProduct(null);
        quantityVo1.setWeight(null);

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(quantityVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(quantityVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Quantity_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenRequested_ByAbsentId() throws Exception {
        String id = String.valueOf(Long.MAX_VALUE);
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = quantityEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(quantityVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getId());
        Assertions.assertEquals(quantityVo1.getWeightId(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getWeightId());
        Assertions.assertEquals(quantityVo1.getProductId(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getProductId());
        Assertions.assertEquals(quantityVo1.getAmount(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getAmount());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getActive()));
    }

    @Test
    public void test_Quantity_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(QUANTITY_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(quantityVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getId());
        Assertions.assertEquals(quantityVo1.getAmount(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getAmount());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getProduct() != null);
        Assertions.assertEquals(quantityVo1.getProduct().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getProduct().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getWeight() != null);
        Assertions.assertEquals(quantityVo1.getWeightId(), om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getWeight().getSymbol());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), QuantityVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Quantity_Delete_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Delete_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = quantityEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Quantity_Delete_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndQuantityDetails_Amount() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        quantityForm = new QuantityForm();
        quantityForm.setAmount(665.0d);

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndQuantityDetails_WeightId() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        quantityForm.setWeightId(Units.GRAM.getSymbol());

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndQuantityDetails_ProductId() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        quantityForm.setProductId(productEntity4.getId().toString());

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Quantity_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenUpdatedBy_EmptyInvalidId_AndQuantityDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByAbsentId_AndQuantityDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_005_WhenUpdated_ByInactiveId_AndQuantityDetails() throws Exception {
        String id = quantityEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndNoQuantityDetails() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
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
    @ValueSource(doubles = { 0.0d, -1.0d })
    public void test_Quantity_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidAmount(Double amount) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "amount";
        quantityForm.setAmount(amount);

        mvcResult = mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Quantity_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndEmptyInvalidWeightId(String weightId) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "weightId";
        quantityForm.setWeightId(weightId);

        mvcResult = mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Quantity_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndEmptyInvalidProductId(String productId) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        quantityForm.setProductId(productId);

        mvcResult = mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999" })
    public void test_Quantity_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndAbsentProductId(String productId) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        quantityForm.setProductId(productId);

        mvcResult = mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInactiveProductId() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String productId = productEntity2.getId().toString();
        String fieldName = "productId";
        quantityForm.setProductId(productId);

        mvcResult = mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(quantityForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Quantity_Put_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndEmptyQuantityDetails() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(QUANTITY_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new QuantityForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Quantity_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndQuantityDetails() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndQuantityDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(QUANTITY_URI_BY_ID, " ")
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByInvalidId_AndQuantityDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_404Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdated_ByAbsentId_AndQuantityDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_422Response_And_ErrorCode_RES_INVENTORY_003_WhenUpdated_ById_AndNoQuantityDetails() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(QUANTITY_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAmount(String amount) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, amount));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    @ValueSource(doubles = { 0.0d, -1.0d })
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidAmount(Double amount) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "amount";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, amount.toString()));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyWeightId(String weightId) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, weightId));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidWeightId(String weightId) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "weightId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, weightId));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyProductId(String productId) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/productId", productId));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidProductId(String productId) throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, productId));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveProductId() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String productId = productEntity2.getId().toString();
        String fieldName = "productId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, productId));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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
    public void test_Quantity_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_ById_AndInvalidDefinitionOfQuantityAttribute() throws Exception {
        String id = quantityEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = InventoryErrorCode.INVENTORY_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(QUANTITY_URI_BY_ID, id)
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