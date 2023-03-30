package com.teenthofabud.restaurant.solution.cookbook;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.repository.CuisineJPARepository;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.integration.inventory.data.ProductVo;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientEntity;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientForm;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientVo;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.repository.IngredientRepository;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import com.teenthofabud.restaurant.solution.cookbook.utils.CookbookServiceHelper;
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
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class IngredientIntegrationTest extends CookbookIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String INGREDIENT_URI = "/ingredient";
    private static final String INGREDIENT_URI_BY_ID = "/ingredient/{id}";
    private static final String INGREDIENT_URI_BY_RECIPE_ID = "/ingredient/recipeid/{recipeId}";
    private static final String INGREDIENT_URI_FILTER = "/ingredient/filter";

    private IngredientRepository ingredientRepository;
    private RecipeRepository recipeRepository;
    private CuisineJPARepository cuisineRepository;
    private CookbookServiceHelper cookbookServiceHelper;

    private int integrationServicePort;

    @Value("${cookbook.integration.service.port}")
    public void setIntegrationServicePort(int integrationServicePort) {
        this.integrationServicePort = integrationServicePort;
    }

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setIngredientRepository(IngredientRepository ingredientRepository) {
        this.ingredientRepository = ingredientRepository;
    }

    @Autowired
    public void setRecipeRepository(RecipeRepository recipeRepository) {
        this.recipeRepository = recipeRepository;
    }

    @Autowired
    public void setCuisineRepository(CuisineJPARepository cuisineRepository) {
        this.cuisineRepository = cuisineRepository;
    }

    private ItemVo itemVo1;
    private ItemVo itemVo2;
    private ItemVo itemVo3;
    private ItemVo itemVo4;

    private ProductVo productVo1;
    private ProductVo productVo2;
    private ProductVo productVo3;
    private ProductVo productVo4;

    private CuisineVo cuisineVo1;
    private CuisineVo cuisineVo2;
    private CuisineVo cuisineVo3;
    private CuisineVo cuisineVo4;
    private CuisineEntity cuisineEntity1;
    private CuisineEntity cuisineEntity2;
    private CuisineEntity cuisineEntity3;
    private CuisineEntity cuisineEntity4;

    private RecipeVo recipeVo1;
    private RecipeVo recipeVo2;
    private RecipeVo recipeVo3;
    private RecipeVo recipeVo4;
    private RecipeEntity recipeEntity1;
    private RecipeEntity recipeEntity2;
    private RecipeEntity recipeEntity3;

    private IngredientForm ingredientForm;
    private IngredientVo ingredientVo1;
    private IngredientVo ingredientVo2;
    private IngredientVo ingredientVo3;
    private IngredientVo ingredientVo4;
    private IngredientEntity ingredientEntity1;
    private IngredientEntity ingredientEntity2;
    private IngredientEntity ingredientEntity3;

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

        productVo1 = new ProductVo();
        productVo1.setActive(Boolean.TRUE);
        productVo1.setName("Product 1");
        productVo1.setId("1");
        productVo1.setDescription("Product 1 description");

        productVo2 = new ProductVo();
        productVo2.setActive(Boolean.TRUE);
        productVo2.setName("Product 2");
        productVo2.setId("2");
        productVo2.setDescription("Product 2 description");

        productVo3 = new ProductVo();
        productVo3.setActive(Boolean.TRUE);
        productVo3.setName("Product 4");
        productVo3.setId("4");
        productVo3.setDescription("Product 4 description");

        productVo4 = new ProductVo();
        productVo4.setActive(Boolean.TRUE);
        productVo4.setName("Product 22");
        productVo4.setId("22");
        productVo4.setDescription("Product 22 description");

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
        cuisineEntity4.setActive(Boolean.TRUE);

        cuisineEntity4 = cuisineRepository.save(cuisineEntity4);

        cuisineVo4 = new CuisineVo();
        cuisineVo4.setId(cuisineEntity4.getId().toString());
        cuisineVo4.setName(cuisineEntity4.getName());
        cuisineVo4.setDescription(cuisineEntity4.getDescription());

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

        ingredientForm = new IngredientForm();
        ingredientForm.setName("New Something First");
        ingredientForm.setDescription("New Something Next");
        ingredientForm.setRecipeId(recipeEntity1.getId().toString());
        ingredientForm.setProductId(productVo1.getId());
        ingredientForm.setQuantityAmount(123.0d);
        ingredientForm.setQuantityUnitId("g");

        ingredientEntity1 = new IngredientEntity();
        ingredientEntity1.setName("Ingredient 1 Name");
        ingredientEntity1.setDescription("Ingredient 1 Description");
        ingredientEntity1.setProductId(productVo1.getId());
        ingredientEntity1.setQuantityAmount(988.7d);
        ingredientEntity1.setQuantityUnitId("g");
        ingredientEntity1.setActive(Boolean.TRUE);
        ingredientEntity1.setRecipe(recipeEntity1);

        ingredientEntity1 = ingredientRepository.save(ingredientEntity1);

        ingredientVo1 = new IngredientVo();
        ingredientVo1.setId(ingredientEntity1.getId().toString());
        ingredientVo1.setName(ingredientEntity1.getName());
        ingredientVo1.setDescription(ingredientEntity1.getDescription());
        ingredientVo1.setProductId(ingredientEntity1.getProductId());
        ingredientVo1.setRecipeId(ingredientEntity1.getRecipe().getId().toString());
        ingredientVo1.setQuantity(ingredientEntity1.getQuantityAmount().toString() + " " + cookbookServiceHelper.parseWeightCode(ingredientEntity1.getQuantityUnitId()).get().getName());
        //ingredientVo1.setCuisine(cuisineVo1);

        ingredientEntity2 = new IngredientEntity();
        ingredientEntity2.setName("Ingredient 2 Name");
        ingredientEntity2.setDescription("Ingredient 2 Description");
        ingredientEntity2.setProductId(productVo2.getId());
        ingredientEntity2.setQuantityAmount(52.7d);
        ingredientEntity2.setQuantityUnitId("kg");
        ingredientEntity2.setActive(Boolean.TRUE);
        ingredientEntity2.setRecipe(recipeEntity2);

        ingredientEntity2 = ingredientRepository.save(ingredientEntity2);

        ingredientVo2 = new IngredientVo();
        ingredientVo2.setId(ingredientEntity2.getId().toString());
        ingredientVo2.setName(ingredientEntity2.getName());
        ingredientVo2.setDescription(ingredientEntity2.getDescription());
        ingredientVo2.setProductId(ingredientEntity2.getProductId());
        ingredientVo2.setRecipeId(ingredientEntity2.getRecipe().getId().toString());
        ingredientVo2.setQuantity(ingredientEntity2.getQuantityAmount().toString() + " " + cookbookServiceHelper.parseWeightCode(ingredientEntity2.getQuantityUnitId()).get().getName());
        //ingredientVo2.setCuisine(cuisineVo2);

        ingredientEntity3 = new IngredientEntity();
        ingredientEntity3.setName("Ingredient 3 Name");
        ingredientEntity3.setDescription("Ingredient 3 Description");
        ingredientEntity3.setProductId(productVo3.getId());
        ingredientEntity3.setQuantityAmount(82.7d);
        ingredientEntity3.setQuantityUnitId("g");
        ingredientEntity3.setActive(Boolean.FALSE);
        ingredientEntity3.setRecipe(recipeEntity3);

        ingredientEntity3 = ingredientRepository.save(ingredientEntity3);

        ingredientVo3 = new IngredientVo();
        ingredientVo3.setId(ingredientEntity3.getId().toString());
        ingredientVo3.setName(ingredientEntity3.getName());
        ingredientVo3.setDescription(ingredientEntity3.getDescription());
        ingredientVo3.setProductId(ingredientEntity3.getProductId());
        ingredientVo3.setRecipeId(ingredientEntity3.getRecipe().getId().toString());
        ingredientVo3.setQuantity(ingredientEntity3.getQuantityAmount().toString() + " " + cookbookServiceHelper.parseWeightCode(ingredientEntity3.getQuantityUnitId()).get().getName());
        //ingredientVo3.setCuisine(cuisineVo3);

        ingredientVo4 = new IngredientVo();
        ingredientVo4.setId(UUID.randomUUID().toString());
        ingredientVo4.setName(ingredientForm.getName());
        ingredientVo4.setDescription(ingredientForm.getDescription());
        ingredientVo4.setProductId(ingredientForm.getProductId());
        ingredientVo4.setRecipeId(ingredientForm.getRecipeId());
        ingredientVo4.setQuantity(ingredientForm.getQuantityAmount().toString() + " " + ingredientForm.getQuantityUnitId());

    }

    @AfterEach
    private void destroy() {
        ingredientEntity1.setRecipe(null);
        ingredientEntity2.setRecipe(null);
        ingredientEntity3.setRecipe(null);

        ingredientRepository.deleteById(ingredientEntity1.getId());
        ingredientRepository.deleteById(ingredientEntity2.getId());
        ingredientRepository.deleteById(ingredientEntity3.getId());

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

    /**
     * POST - start
     */

    /**
     * POST with form - START
     */

    @Test
    public void test_Ingredient_Post_ShouldReturn_201Response_And_NewIngredientId_WhenPosted_WithValidIngredientForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(INGREDIENT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Ingredient_Post_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenPosted_WithNoIngredientForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    /**
     * POST with form - END
     */

    /**
     * POST with empty fields - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyName(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        ingredientForm.setName(name);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyProductId(String productId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        ingredientForm.setProductId(productId);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyRecipeId(String recipeId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        ingredientForm.setRecipeId(recipeId);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyQuantityAmount() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityAmount";
        ingredientForm.setQuantityAmount(null);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyQuantityUnitId(String quantityUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityUnitId";
        ingredientForm.setQuantityUnitId(quantityUnitId);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * POST with empty fields - END
     */

    /**
     * POST with invalid fields - START
     */

    @Test
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenRequested_WithInvalidProductId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-INVENTORY-001";
        String fieldName = "id";
        ingredientForm.setProductId("r");

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidRecipeId(String recipeId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        ingredientForm.setRecipeId(recipeId);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidQuantityAmount(Double quantityAmount) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityAmount";
        ingredientForm.setQuantityAmount(quantityAmount);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidQuantityUnitId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityUnitId";
        ingredientForm.setQuantityUnitId("r");

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * POST with invalid fields - END
     */

    /**
     * POST with absent fields - START
     */


    @Test
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenRequested_WithAbsentProductId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-INVENTORY-002";
        String fieldName = "id";
        String keyword = "unavailable";
        ingredientForm.setProductId("3");

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_Ingredient_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithAbsentRecipeId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        ingredientForm.setRecipeId(String.valueOf(Long.MAX_VALUE));

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * POST with absent fields - END
     */

    /**
     * POST with duplicate fields - START
     */

    @Test
    public void test_Ingredient_Post_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenRequested_WithDuplicateIngredient() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "recipeId";
        String field3Name = "productId";
        String field1Value = ingredientEntity2.getName();
        String field2Value = ingredientEntity2.getRecipe().getId().toString();
        String field3Value = ingredientEntity2.getProductId();
        ingredientForm.setName(field1Value);
        ingredientForm.setRecipeId(field2Value);
        ingredientForm.setProductId(field3Value);

        mvcResult = mockMvc.perform(post(INGREDIENT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Value));
    }

    /**
     * POST with duplicate fields - END
     */

    /**
     * POST - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * PUT - start
     */

    /**
     * PUT with form - START
     */

    @Test
    public void test_Ingredient_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdatedById_WithValidIngredientForm() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        ingredientForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Put_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdatedById_WithNoIngredientForm() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    /**
     * PUT with form - END
     */

    /**
     * PUT with empty fields - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyName(String name) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        ingredientForm.setName(name);

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyProductId(String productId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "productId";
        ingredientForm.setProductId(productId);

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyRecipeId(String recipeId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        ingredientForm.setRecipeId(recipeId);

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * Empty checks against fields of primitive types not possible
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyQuantityUnitId(String quantityUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityUnitId";
        ingredientForm.setQuantityUnitId(quantityUnitId);

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PUT with empty fields - END
     */

    /**
     * PUT with invalid fields - START
     */

    @Test
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenUpdatedById_WithInvalidProductId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = "RES-INVENTORY-001";
        String fieldName = "id";
        ingredientForm.setProductId("r");

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidRecipeId(String recipeId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        ingredientForm.setRecipeId(recipeId);

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidQuantityAmount(Double quantityAmount) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityAmount";
        ingredientForm.setQuantityAmount(quantityAmount);

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidQuantityUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityUnitId";
        ingredientForm.setQuantityUnitId("r");

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PUT with invalid fields - END
     */

    /**
     * PUT with absent fields - START
     */

    @Test
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_002_WhenUpdatedById_WithAbsentProductId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = "RES-INVENTORY-002";
        String fieldName = "id";
        String keyword = "unavailable";
        ingredientForm.setProductId("3");

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_Ingredient_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithAbsentRecipeId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        ingredientForm.setRecipeId(String.valueOf(Long.MAX_VALUE));

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PUT with absent fields - END
     */

    /**
     * PUT with duplicate fields - START
     */

    @Test
    public void test_Ingredient_Put_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenUpdatedById_WithDuplicateIngredient() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "recipeId";
        String field3Name = "productId";
        String field1Value = ingredientEntity2.getName();
        String field2Value = ingredientEntity2.getRecipe().getId().toString();
        String field3Value = ingredientEntity2.getProductId();
        ingredientForm.setName(field1Value);
        ingredientForm.setRecipeId(field2Value);
        ingredientForm.setProductId(field3Value);

        mvcResult = mockMvc.perform(put(INGREDIENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(ingredientForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Value));
    }

    /**
     * PUT with duplicate fields - END
     */

    /**
     * PUT - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * PATCH - start
     */

    /**
     * PATCH with form - START
     */

    @Test
    public void test_Ingredient_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdatedById_WithValidIngredientAttributes() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();

        mvcResult = this.mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Patch_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdatedById_WithNoIngredientAttributes() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /**
     * PATCH with form - END
     */

    /**
     * PATCH with empty fields - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyName(String name) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", name));


        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyProductId(String productId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/productId", productId));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyRecipeId(String recipeId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/recipeId", recipeId));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * Empty checks against fields of primitive types not possible
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyQuantityUnitId(String quantityUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/quantityUnitId", quantityUnitId));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PATCH with empty fields - END
     */

    /**
     * PATCH with invalid fields - START
     */

    @Test
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidDefinitionOfIngredientAttribute() throws Exception {
        String id = ingredientEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
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
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_RES_INVENTORY_001_WhenUpdatedById_WithInvalidProductId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = "RES-INVENTORY-001";
        String fieldName = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/productId", "r"));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidRecipeId(String recipeId) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, recipeId));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
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
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidQuantityAmount(Double quantityAmount) throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityAmount";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, quantityAmount.toString()));


        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
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
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidQuantityUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityUnitId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "r"));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PATCH with invalid fields - END
     */

    /**
     * PATCH with absent fields - START
     */

    @Test
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithAbsentProductId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        String keyword = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "3"));


        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_Ingredient_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithAbsentRecipeId() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, String.valueOf(Long.MAX_VALUE)));


        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /**
     * PATCH with absent fields - END
     */

    /**
     * PATCH with duplicate fields - START
     */

    @Test
    public void test_Ingredient_Patch_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenUpdatedById_WithDuplicateIngredient() throws Exception {
        MvcResult mvcResult = null;
        String id = ingredientEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "recipeId";
        String field3Name = "productId";
        String field1Value = ingredientEntity2.getName();
        String field2Value = ingredientEntity2.getRecipe().getId().toString();
        String field3Value = ingredientEntity2.getProductId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value),
                new PatchOperationForm("replace", "/" + field3Name, field3Value));

        mvcResult = mockMvc.perform(patch(INGREDIENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Value));
    }

    /**
     * PATCH with duplicate fields - END
     */

    /**
     * PATCH - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * DELETE - START
     */

    @Test
    public void test_Ingredient_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = ingredientEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(INGREDIENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Ingredient_Delete_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001__WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(INGREDIENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Ingredient_Delete_ShouldReturn_400Response_And_ErrorCode_RES_COOK_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = ingredientEntity3.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(INGREDIENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Ingredient_Delete_ShouldReturn_404Response_And_ErrorCode_RES_COOK_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(INGREDIENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /**
     * DELETE - END
     */

    /**
     * =========================================================================================================================================
     */

    /**
     * GET - START
     */

    /**
     * GET all - START
     */

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_IngredientListNaturallyOrdered_WhenRequested_ForAllIngredients() throws Exception {
        MvcResult mvcResult = null;
        List<IngredientVo> ingredientList = new ArrayList<>(Arrays.asList(ingredientVo1, ingredientVo2, ingredientVo3));

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
    }

    /**
     * GET all - END
     */

    /**
     * GET all by path variable - START
     */

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_IngredientDetails_WhenRequested_ById() throws Exception {
        String id = ingredientEntity1.getId().toString();
        MvcResult mvcResult = null;
        ingredientVo1.setRecipe(null);

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(ingredientVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(ingredientVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = ingredientEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getId());
        Assertions.assertEquals(ingredientVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getName());
        Assertions.assertEquals(ingredientVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getDescription());
        Assertions.assertEquals(ingredientVo1.getProductId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getProductId());
        Assertions.assertEquals(ingredientVo1.getRecipeId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getRecipeId());
        Assertions.assertEquals(ingredientVo1.getQuantity(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getQuantity());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getActive()));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = ingredientEntity1.getId().toString();
        MvcResult mvcResult = null;
        ingredientVo1.setProduct(productVo1);
        ingredientVo1.setRecipe(recipeVo1);

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getId());
        Assertions.assertEquals(ingredientVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getName());
        Assertions.assertEquals(ingredientVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getDescription());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getProduct() != null);
        Assertions.assertEquals(ingredientVo1.getProduct().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getProduct().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getRecipe() != null);
        Assertions.assertEquals(ingredientVo1.getRecipe().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getRecipe().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getActive()));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_IngredientListNaturallyOrdered_WhenRequested_ForIngredients_ByRecipeId() throws Exception {
        MvcResult mvcResult = null;

        List<IngredientVo> ingredientList = Arrays.asList(ingredientVo1);
        ingredientVo1.setRecipe(null);

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_RECIPE_ID, recipeEntity1.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(ingredientList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_IngredientListNaturallyOrdered_WhenRequested_ForIngredients_ByEmptyRecipeId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String recipeId = " ";
        String fieldName = "recipeId";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_RECIPE_ID, recipeId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_404Response_And_ErrorCode_RES_COOK_001_WhenRequested_ByInvalidRecipeId() throws Exception {
        MvcResult mvcResult = null;
        String recipeId = "kk";
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_RECIPE_ID, recipeId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(recipeId));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_404Response_And_ErrorCode_RES_COOK_001_WhenRequested_ByAbsentRecipeId() throws Exception {
        MvcResult mvcResult = null;
        String recipeId = String.valueOf(Long.MAX_VALUE);
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_RECIPE_ID, recipeId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(recipeId));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_404Response_And_ErrorCode_RES_COOK_001_WhenRequested_ByInactiveRecipeId() throws Exception {
        MvcResult mvcResult = null;
        String recipeId = recipeEntity3.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "recipeId";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_BY_RECIPE_ID, recipeId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(recipeId));
    }

    /**
     * GET all by path variable - END
     */

    /**
     * GET all by empty filters - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyProductIdOnly(String productId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("productId", productId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyQuantityAmountOnly(String quantityAmount) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("quantityAmount", quantityAmount))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyQuantityUnitIdOnly(String quantityUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("quantityUnitId", quantityUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /**
     * GET all by empty filters - END
     */

    /**
     * GET all by invalid filters - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "r", "-1", "0" })
    public void test_Ingredient_Get_ShouldReturn_500Response_And_ErrorCode_RES_INVENTORY_001_WhenRequestedBy_InvalidProductIdOnly(String quantityAmount) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityAmount";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam(fieldName, quantityAmount.toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidQuantityAmountOnly(Double quantityAmount) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "quantityAmount";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam(fieldName, quantityAmount.toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidQuantityUnitIdOnly() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String quantityUnitId = "r";
        String fieldName = "quantityUnitId";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam(fieldName, quantityUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /**
     * GET all by invalid filters - END
     */

    /**
     * GET all by absent filters - START
     */

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_EmptyIngredientList_WhenRequestedBy_AbsentNameOnly() throws Exception {
        MvcResult mvcResult = null;
        String name = "x";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_EmptyIngredientList_WhenRequestedBy_AbsentDescriptionOnly() throws Exception {
        MvcResult mvcResult = null;
        String description = "x";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_500Response_And_ErrorCode_RES_INVENTORY_002_WhenRequestedBy_AbsentProductIdOnly() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-INVENTORY-002";
        String productId = "3";
        String fieldName = "id";


        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("productId", productId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_EmptyIngredientList_WhenRequestedBy_AbsentQuantityAmountAndValidQuantityUnitId() throws Exception {
        MvcResult mvcResult = null;
        String quantityAmount = String.valueOf(Double.MAX_VALUE);
        String quantityUnitId = "g";

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER).queryParam("quantityAmount", quantityAmount)
                        .queryParam("quantityUnitId", quantityUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
    }

    /**
     * GET all by absent filters - END
     */

    /**
     * GET all by filter combination - START
     */

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForIngredients_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<IngredientVo> ingredientList = new ArrayList<>(Arrays.asList(ingredientVo1, ingredientVo2, ingredientVo3));
        ingredientVo1.setRecipe(null);

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER)
                        .queryParam("name", "Ingredient"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(ingredientList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForIngredients_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<IngredientVo> ingredientList = new ArrayList<>(Arrays.asList(ingredientVo2));
        ingredientVo2.setRecipe(null);

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER)
                        .queryParam("description", "Ingredient 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(ingredientList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForIngredients_WithProductId() throws Exception {
        MvcResult mvcResult = null;
        recipeVo1.setCuisine(null);

        List<IngredientVo> ingredientList = new ArrayList<>(Arrays.asList(ingredientVo1));
        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER)
                        .queryParam("productId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(ingredientList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForIngredients_WithQuantityAmountAndPortionSizeUnitId() throws Exception {
        MvcResult mvcResult = null;
        List<IngredientVo> ingredientList = new ArrayList<>(Arrays.asList(ingredientVo3));
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);


        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER)
                        .queryParam("quantityAmount", "82.7")
                        .queryParam("quantityUnitId", "g"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(ingredientList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForIngredients_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<IngredientVo> ingredientList = Arrays.asList(ingredientVo1, ingredientVo2, ingredientVo3);
        ingredientVo1.setRecipe(null);
        ingredientVo2.setRecipe(null);
        ingredientVo3.setRecipe(null);

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER)
                        .queryParam("name", "Ingredient")
                        .queryParam("description", "Ingredient"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(ingredientList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForIngredients_WithNameAndDescriptionAndProductId() throws Exception {
        MvcResult mvcResult = null;
        List<IngredientVo> ingredientList = Arrays.asList(ingredientVo3);
        ingredientVo3.setRecipe(null);

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER)
                        .queryParam("name", "Ingredient 3")
                        .queryParam("description", "Ingredient 3")
                        .queryParam("productId", "4"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(ingredientList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(ingredientList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Ingredient_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequested_ForIngredients_WithAbsent_DescriptionAndProductIdAndCookingMethod() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-INVENTORY-002";
        String fieldName = "id";
        List<IngredientVo> ingredientList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(INGREDIENT_URI_FILTER)
                        .queryParam("description", "Recipe 1")
                        .queryParam("productId", "3"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /**
     * GET all by filter combination - END
     */

    @Override
    public String getSimulationBaseLocation() {
        return "simulation/inventory-service";
    }

    @Override
    public Integer getServicePort() {
        return this.integrationServicePort;
    }

    @Override
    public String[] getSimulationFilePaths() {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation-v3.json") };
    }
}
