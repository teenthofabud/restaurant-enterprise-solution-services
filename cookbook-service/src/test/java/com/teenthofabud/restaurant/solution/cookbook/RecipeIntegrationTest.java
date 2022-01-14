package com.teenthofabud.restaurant.solution.cookbook;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineForm;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.repository.CuisineRepository;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.data.CityVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.data.CountryVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.product.data.StateVo;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
public class RecipeIntegrationTest extends CookbookIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ADDRESS_URI = "/recipe";
    private static final String ADDRESS_URI_BY_ID = "/recipe/{id}";
    private static final String ADDRESS_URI_BY_ACCOUNT_ID = "/recipe/cuisineid/{cuisineId}";
    private static final String ADDRESS_URI_FILTER = "/recipe/filter";

    private RecipeRepository recipeRepository;
    private CuisineRepository cuisineRepository;

    private int inventoryServicePort;

    @Value("${cookbook.inventory.service.port}")
    public void setInventoryServicePort(int inventoryServicePort) {
        this.inventoryServicePort = inventoryServicePort;
    }

    @Autowired
    public void setRecipeRepository(RecipeRepository recipeRepository) {
        this.recipeRepository = recipeRepository;
    }

    @Autowired
    public void setCuisineRepository(CuisineRepository cuisineRepository) {
        this.cuisineRepository = cuisineRepository;
    }

    private ItemVo itemVo1;
    private ItemVo itemVo2;
    private ItemVo itemVo3;
    private ItemVo itemVo4;

    private CuisineVo cuisineVo1;
    private CuisineVo cuisineVo2;
    private CuisineVo cuisineVo3;
    private CuisineVo cuisineVo4;
    private CuisineEntity cuisineEntity1;
    private CuisineEntity cuisineEntity2;
    private CuisineEntity cuisineEntity3;
    private CuisineEntity cuisineEntity4;

    private RecipeForm recipeForm;
    private RecipeVo recipeVo1;
    private RecipeVo recipeVo2;
    private RecipeVo recipeVo3;
    private RecipeVo recipeVo4;
    private RecipeEntity recipeEntity1;
    private RecipeEntity recipeEntity2;
    private RecipeEntity recipeEntity3;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        itemVo1 = new ItemVo();
        itemVo1.setActive(Boolean.TRUE);
        itemVo1.setName("Item 1");
        itemVo1.setId("1");
        itemVo1.setDescription("Item 1 description");

        itemVo2 = new ItemVo();
        itemVo2.setActive(Boolean.TRUE);
        itemVo2.setName("Item 2");
        itemVo2.setId("2");
        itemVo2.setDescription("Item 2 description");

        itemVo3 = new ItemVo();
        itemVo3.setActive(Boolean.TRUE);
        itemVo3.setName("Item 4");
        itemVo3.setId("4");
        itemVo3.setDescription("Item 4 description");

        itemVo4 = new ItemVo();
        itemVo4.setActive(Boolean.TRUE);
        itemVo4.setName("Item 22");
        itemVo4.setId("22");
        itemVo4.setDescription("Item 22 description");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"));


        cuisineEntity1 = new CuisineEntity();
        cuisineEntity1.setName("Cuisine 1 Name");
        cuisineEntity1.setDescription("Cuisine 1 Description");
        cuisineEntity1.setActive(Boolean.TRUE);

        cuisineEntity1 = cuisineRepository.save(cuisineEntity1);

        cuisineVo1 = new CuisineVo();
        cuisineVo1.setId(cuisineEntity1.getId().toString());
        cuisineVo1.setName(cuisineEntity1.getName());
        cuisineVo1.setDescription(cuisineEntity1.getDescription());

        cuisineEntity2 = new CuisineEntity();
        cuisineEntity2.setName("Cuisine 2 Name");
        cuisineEntity2.setDescription("Cuisine 2 Description");
        cuisineEntity2.setActive(Boolean.TRUE);

        cuisineEntity2 = cuisineRepository.save(cuisineEntity2);

        cuisineVo2 = new CuisineVo();
        cuisineVo2.setId(cuisineEntity2.getId().toString());
        cuisineVo2.setName(cuisineEntity2.getName());
        cuisineVo2.setDescription(cuisineEntity2.getDescription());

        cuisineEntity3 = new CuisineEntity();
        cuisineEntity3.setName("Cuisine 3 Name");
        cuisineEntity3.setDescription("Cuisine 3 Description");
        cuisineEntity3.setActive(Boolean.FALSE);

        cuisineEntity3 = cuisineRepository.save(cuisineEntity3);

        cuisineVo3 = new CuisineVo();
        cuisineVo3.setId(cuisineEntity3.getId().toString());
        cuisineVo3.setName(cuisineEntity3.getName());
        cuisineVo3.setDescription(cuisineEntity3.getDescription());

        cuisineEntity4 = new CuisineEntity();
        cuisineEntity4.setName("Cuisine 4 Name");
        cuisineEntity4.setDescription("Cuisine 4 Description");
        cuisineEntity4.setActive(Boolean.FALSE);

        cuisineEntity4 = cuisineRepository.save(cuisineEntity4);

        cuisineVo4 = new CuisineVo();
        cuisineVo4.setId(cuisineEntity4.getId().toString());
        cuisineVo4.setName(cuisineEntity4.getName());
        cuisineVo4.setDescription(cuisineEntity4.getDescription());

        recipeForm = new RecipeForm();
        recipeForm.setName("New Something First");
        recipeForm.setDescription("New Something Next");
        recipeForm.setCuisineId(cuisineEntity4.getId().toString());
        recipeForm.setCookingMethod("new cooking method");
        recipeForm.setInstructions("new instructions");
        recipeForm.setNumberOfServings(2);
        recipeForm.setItemId(itemVo4.getId());
        recipeForm.setCookingTimeDuration(2.2d);
        recipeForm.setCookingTimeUnitId("m");
        recipeForm.setPreparationTimeDuration(22.2d);
        recipeForm.setPreparationTimeUnitId("m");
        recipeForm.setPortionSizeAmount(123.0d);
        recipeForm.setPortionSizeUnitId("g");

        recipeEntity1 = new RecipeEntity();
        recipeEntity1.setName("default");
        recipeEntity1.setDescription("Recipe 1 Name");
        recipeEntity1.setInstructions("Recipe 1 Description");
        recipeEntity1.setCookingMethod("IN");
        recipeEntity1.setNumberOfServings(5);
        recipeEntity1.setItemId(itemVo1.getId());
        recipeEntity1.setCookingTimeDuration(53.9d);
        recipeEntity1.setCookingTimeUnitId("m");
        recipeEntity1.setPreparationTimeDuration(40.9d);
        recipeEntity1.setPreparationTimeUnitId("m");
        recipeEntity1.setPortionSizeAmount(988.7d);
        recipeEntity1.setPortionSizeUnitId("g");
        recipeEntity1.setActive(Boolean.TRUE);
        recipeEntity1.setCuisine(cuisineEntity1);

        recipeEntity1 = recipeRepository.save(recipeEntity1);

        recipeVo1 = new RecipeVo();
        recipeVo1.setId(recipeEntity1.getId().toString());
        recipeVo1.setName(recipeEntity1.getName());
        recipeVo1.setDescription(recipeEntity1.getDescription());
        recipeVo1.setInstructions(recipeEntity1.getInstructions());
        recipeVo1.setCookingMethod(recipeEntity1.getCookingMethod());
        recipeVo1.setNumberOfServings(recipeEntity1.getNumberOfServings());
        recipeVo1.setItemId(recipeEntity1.getItemId());
        recipeVo1.setCuisineId(recipeEntity1.getCuisine().getId().toString());
        recipeVo1.setPreparationTime(recipeEntity1.getPreparationTimeDuration().toString() + " " + recipeEntity1.getPreparationTimeUnitId());
        recipeVo1.setCookingTime(recipeEntity1.getCookingTimeDuration().toString() + " " + recipeEntity1.getCookingTimeUnitId());
        recipeVo1.setPortionSize(recipeEntity1.getPortionSizeAmount().toString() + " " + recipeEntity1.getPortionSizeUnitId());
        //recipeVo1.setCuisine(cuisineVo1);

        recipeEntity2 = new RecipeEntity();
        recipeEntity2.setName("default");
        recipeEntity2.setDescription("Recipe 2 Name");
        recipeEntity2.setInstructions("Recipe 2 Description");
        recipeEntity2.setCookingMethod("IN");
        recipeEntity2.setNumberOfServings(1);
        recipeEntity2.setItemId(itemVo2.getId());
        recipeEntity2.setCookingTimeDuration(19.2d);
        recipeEntity2.setCookingTimeUnitId("m");
        recipeEntity2.setPreparationTimeDuration(43.2d);
        recipeEntity2.setPreparationTimeUnitId("m");
        recipeEntity2.setPortionSizeAmount(52.7d);
        recipeEntity2.setPortionSizeUnitId("kg");
        recipeEntity2.setActive(Boolean.TRUE);
        recipeEntity2.setCuisine(cuisineEntity2);

        recipeEntity2 = recipeRepository.save(recipeEntity2);

        recipeVo2 = new RecipeVo();
        recipeVo2.setId(recipeEntity2.getId().toString());
        recipeVo2.setName(recipeEntity2.getName());
        recipeVo2.setDescription(recipeEntity2.getDescription());
        recipeVo2.setInstructions(recipeEntity2.getInstructions());
        recipeVo2.setCookingMethod(recipeEntity2.getCookingMethod());
        recipeVo2.setNumberOfServings(recipeEntity2.getNumberOfServings());
        recipeVo2.setItemId(recipeEntity2.getItemId());
        recipeVo2.setCuisineId(recipeEntity2.getCuisine().getId().toString());
        recipeVo2.setPreparationTime(recipeEntity2.getPreparationTimeDuration().toString() + " " + recipeEntity2.getPreparationTimeUnitId());
        recipeVo2.setCookingTime(recipeEntity2.getCookingTimeDuration().toString() + " " + recipeEntity2.getCookingTimeUnitId());
        recipeVo2.setPortionSize(recipeEntity2.getPortionSizeAmount().toString() + " " + recipeEntity2.getPortionSizeUnitId());
        //recipeVo2.setCuisine(cuisineVo2);

        recipeEntity3 = new RecipeEntity();
        recipeEntity3.setName("default");
        recipeEntity3.setDescription("Recipe 3 Name");
        recipeEntity3.setInstructions("Recipe 3 Description");
        recipeEntity3.setCookingMethod("IN");
        recipeEntity3.setNumberOfServings(4);
        recipeEntity3.setItemId(itemVo3.getId());
        recipeEntity3.setCookingTimeDuration(59.2d);
        recipeEntity3.setCookingTimeUnitId("m");
        recipeEntity3.setPreparationTimeDuration(44.2d);
        recipeEntity3.setPreparationTimeUnitId("m");
        recipeEntity3.setPortionSizeAmount(82.7d);
        recipeEntity3.setPortionSizeUnitId("g");
        recipeEntity3.setActive(Boolean.FALSE);
        recipeEntity3.setCuisine(cuisineEntity3);

        recipeEntity3 = recipeRepository.save(recipeEntity3);

        recipeVo3 = new RecipeVo();
        recipeVo3.setId(recipeEntity3.getId().toString());
        recipeVo3.setName(recipeEntity3.getName());
        recipeVo3.setDescription(recipeEntity3.getDescription());
        recipeVo3.setInstructions(recipeEntity3.getInstructions());
        recipeVo3.setCookingMethod(recipeEntity3.getCookingMethod());
        recipeVo3.setNumberOfServings(recipeEntity3.getNumberOfServings());
        recipeVo3.setItemId(recipeEntity3.getItemId());
        recipeVo3.setCuisineId(recipeEntity3.getCuisine().getId().toString());
        recipeVo3.setPreparationTime(recipeEntity3.getPreparationTimeDuration().toString() + " " + recipeEntity3.getPreparationTimeUnitId());
        recipeVo3.setCookingTime(recipeEntity3.getCookingTimeDuration().toString() + " " + recipeEntity3.getCookingTimeUnitId());
        recipeVo3.setPortionSize(recipeEntity3.getPortionSizeAmount().toString() + " " + recipeEntity3.getPortionSizeUnitId());
        //recipeVo3.setCuisine(cuisineVo3);

        recipeVo4 = new RecipeVo();
        recipeVo4.setId(UUID.randomUUID().toString());
        recipeVo4.setName(recipeForm.getName());
        recipeVo4.setDescription(recipeForm.getDescription());
        recipeVo4.setInstructions(recipeForm.getInstructions());
        recipeVo4.setCookingMethod(recipeForm.getCookingMethod());
        recipeVo4.setNumberOfServings(recipeForm.getNumberOfServings());
        recipeVo4.setItemId(recipeForm.getItemId());
        recipeVo4.setCuisineId(recipeForm.getCuisineId());
        recipeVo4.setPreparationTime(recipeForm.getPreparationTimeDuration().toString() + " " + recipeForm.getPreparationTimeUnitId());
        recipeVo4.setCookingTime(recipeForm.getCookingTimeDuration().toString() + " " + recipeForm.getCookingTimeUnitId());
        recipeVo4.setPortionSize(recipeForm.getPortionSizeAmount().toString() + " " + recipeForm.getPortionSizeUnitId());

    }

    @AfterEach
    private void destroy() {
        recipeEntity1.setCuisine(null);
        recipeEntity2.setCuisine(null);
        recipeEntity3.setCuisine(null);

        recipeRepository.deleteById(recipeEntity1.getId());
        recipeRepository.deleteById(recipeEntity2.getId());
        recipeRepository.deleteById(recipeEntity3.getId());

        cuisineRepository.deleteById(cuisineEntity1.getId());
        cuisineRepository.deleteById(cuisineEntity2.getId());
        cuisineRepository.deleteById(cuisineEntity3.getId());
        cuisineRepository.deleteById(cuisineEntity4.getId());
    }

    @Test
    public void test_Recipe_Post_ShouldReturn_201Response_And_NewRecipeId_WhenPosted_WithValidRecipeForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ADDRESS_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        recipeForm.setDescription("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyCityId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cityId";
        recipeForm.setCityId("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidCityId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cityId";
        recipeForm.setCityId("r");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyPincode() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "pincode";
        recipeForm.setPincode("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyStateId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "stateId";
        recipeForm.setStateId("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidStateId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "state";
        recipeForm.setStateId("11r");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyCountryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "countryId";
        recipeForm.setCountryId("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidCountryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "country";
        recipeForm.setCountryId("11r");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId("r");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithAbsentCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId("99999");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenRequested_WithDuplicateRecipe() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "cuisineId";
        String field1Value = recipeForm.getName();
        String field2Value = recipeEntity2.getCuisine().getId().toString();
        recipeForm.setCuisineId(recipeEntity2.getCuisine().getId().toString());

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Post_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenPosted_WithNoRecipeForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
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
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForAllRecipees() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_ByCuisineId() throws Exception {
        MvcResult mvcResult = null;

        List<RecipeVo> recipeList = Arrays.asList(recipeVo1);
        recipeVo1.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ACCOUNT_ID, cuisineEntity1.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_ByEmptyCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ACCOUNT_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_404Response_And_ErrorCode_RES_COOK_001_WhenRequested_ByAbsentCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String albumId = "kk";
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ACCOUNT_ID, albumId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(albumId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyPincodeOnly(String pincode) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("pincode", pincode))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyCityIdOnly(String cityId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("cityId", cityId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyStateIdOnly(String stateId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("stateId", stateId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyCountryIdOnly(String countryId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("countryId", countryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentDescriptionName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentPincode() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("pincode", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentCityId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("cityId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentStateId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("stateId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentCountryId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("countryId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }


    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1));
        recipeVo1.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("description", "Recipe 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithInstructions() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1));
        recipeVo1.setCuisine(null);
        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("instructions", "Recipe 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithPincode() throws Exception {
        MvcResult mvcResult = null;
        recipeVo2.setCuisine(null);
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo2));

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("pincode", "200111"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithCityId() throws Exception {
        MvcResult mvcResult = null;
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));
        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", "133024"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithStateId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);


        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("stateId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("countryId", "IN"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithStateIdAndCityId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo1, recipeVo2, recipeVo3);
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", "133024")
                        .queryParam("stateId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithCountryIdAndStateId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo1, recipeVo2, recipeVo3);
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("countryId", "IN")
                        .queryParam("stateId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithCountryIdAndCityId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo1, recipeVo2, recipeVo3);
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("countryId", "IN")
                        .queryParam("cityId", "133024"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithCityIdAndPincode() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo2);
        recipeVo2.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", "133024")
                        .queryParam("pincode", "200111"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipees_WithDescriptionAndPincodeAndCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo2);
        recipeVo2.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("description", "Recipe 2")
                        .queryParam("pincode", "200111")
                        .queryParam("countryId", "IN"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequested_ForRecipees_WithAbsent_DescriptionAndCityIdAndCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("description", "Recipe 1")
                        .queryParam("cityId", UUID.randomUUID().toString())
                        .queryParam("countryId", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequested_ForRecipees_WithAbsent_CityIdAndStateIdAndCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", UUID.randomUUID().toString())
                        .queryParam("stateId", UUID.randomUUID().toString())
                        .queryParam("countryId", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeDetails_WhenRequested_ById() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        recipeVo1.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(recipeVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(recipeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = recipeEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getId());
        Assertions.assertEquals(recipeVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getDescription());
        Assertions.assertEquals(recipeVo1.getInstructions(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getInstructions());
        Assertions.assertEquals(recipeVo1.getPincode(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getPincode());
        Assertions.assertEquals(recipeVo1.getCityId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCityId());
        Assertions.assertEquals(recipeVo1.getStateId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getStateId());
        Assertions.assertEquals(recipeVo1.getCountryId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCountryId());
        Assertions.assertEquals(recipeVo1.getCuisineId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCuisineId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getActive()));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        recipeVo1.setCity(cityVo);
        recipeVo1.setState(stateVo);
        recipeVo1.setCountry(countryVo);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getId());
        Assertions.assertEquals(recipeVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getDescription());
        Assertions.assertEquals(recipeVo1.getInstructions(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getInstructions());
        Assertions.assertEquals(recipeVo1.getPincode(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getPincode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCity() != null);
        Assertions.assertEquals(recipeVo1.getCity().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCity().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getState() != null);
        Assertions.assertEquals(recipeVo1.getState().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getState().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCountry() != null);
        Assertions.assertEquals(recipeVo1.getCountry().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCountry().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCuisine() != null);
        Assertions.assertEquals(recipeVo1.getCuisine().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCuisine().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Recipe_Delete_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001__WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Delete_ShouldReturn_400Response_And_ErrorCode_RES_COOK_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = recipeEntity3.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Recipe_Delete_ShouldReturn_404Response_And_ErrorCode_RES_COOK_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndRecipeDetails() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        recipeForm.setDescription("Ferran");

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedBy_EmptyInvalidId_AndRecipeDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Put_ShouldReturn_404Response_And_ErrorCode_RES_COOK_002_WhenUpdated_ByAbsentId_AndRecipeDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_005_WhenUpdated_ByInactiveId_AndRecipeDetails() throws Exception {
        String id = recipeEntity3.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Recipe_Put_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdated_ById_AndNoRecipeDetails() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndEmptyDescription(String description) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        recipeForm.setDescription(description);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndEmptyInvalidCityId(String cityId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cityId";
        recipeForm.setCityId(cityId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndEmptyInvalidCuisineId(String cuisineId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(cuisineId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndAbsentCuisineId(String cuisineId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(cuisineId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndEmptyStateId(String stateId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "state";
        recipeForm.setStateId(stateId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidStateId(String stateId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "state";
        recipeForm.setStateId(stateId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndEmptyCountryId(String countryId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "country";
        recipeForm.setCountryId(countryId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidCountryId(String countryId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "country";
        recipeForm.setCountryId(countryId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));
    }

    @Test
    public void test_Recipe_Put_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdated_ById_AndEmptyRecipeDetails() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new RecipeForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Recipe_Put_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenUpdated_ById_AndDuplicateRecipeDetails() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "cuisineId";
        String field1Value = recipeForm.getName();
        String field2Value = recipeEntity2.getCuisine().getId().toString();
        recipeForm.setCuisineId(field2Value);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndRecipeDetails() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndRecipeDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, " ")
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_002_WhenUpdated_ByInvalidId_AndRecipeDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_404Response_And_ErrorCode_RES_COOK_002_WhenUpdated_ByAbsentId_AndRecipeDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_409Response_And_ErrorCode_RES_COOK_002_WhenUpdated_ById_AndDuplicateRecipeDetails() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "cuisineId";
        String field1Value = "default";
        String field2Value = recipeEntity2.getCuisine().getId().toString();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value));


        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdated_ById_AndNoRecipeDetails() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCityId(String cityId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/cityId", cityId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidCityId(String cityId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cityId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, cityId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyStateId(String stateId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, stateId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidStateId(String stateId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "state";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/countryId", recipeEntity1.getCountryId()),
                new PatchOperationForm("replace", "/stateId", stateId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCountryId(String countryId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/countryId", countryId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidCountryId(String countryId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "country";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/countryId", countryId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().toLowerCase().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCuisineId(String cuisineId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/cuisineId", cuisineId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidCuisineId(String cuisineId) throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, cuisineId));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidDefinitionOfRecipeAttribute() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public String getSimulationBaseLocation() {
        return "simulation/inventory-service";
    }

    @Override
    public Integer getServicePort() {
        return this.inventoryServicePort;
    }

    @Override
    public String[] getSimulationFilePaths() {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation-v3.json") };
    }
}
