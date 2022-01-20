package com.teenthofabud.restaurant.solution.cookbook;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.repository.CuisineRepository;
import com.teenthofabud.restaurant.solution.cookbook.ingredient.data.IngredientVo;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
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
public class RecipeIntegrationTest extends CookbookIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String RECIPE_URI = "/recipe";
    private static final String RECIPE_URI_BY_ID = "/recipe/{id}";
    private static final String RECIPE_URI_BY_CUISINE_ID = "/recipe/cuisineid/{cuisineId}";
    private static final String RECIPE_URI_FILTER = "/recipe/filter";

    private RecipeRepository recipeRepository;
    private CuisineRepository cuisineRepository;

    private int integrationServicePort;

    @Value("${cookbook.integration.service.port}")
    public void setIntegrationServicePort(int integrationServicePort) {
        this.integrationServicePort = integrationServicePort;
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
        cuisineEntity4.setActive(Boolean.TRUE);

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

    /**
     * POST - start
     */

    /**
     * POST with form - START
     */

    @Test
    public void test_Recipe_Post_ShouldReturn_201Response_And_NewRecipeId_WhenPosted_WithValidRecipeForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(RECIPE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Recipe_Post_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenPosted_WithNoRecipeForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyName(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        recipeForm.setName(name);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyItemId(String itemId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        recipeForm.setItemId(itemId);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyInstructions(String instructions) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "instructions";
        recipeForm.setInstructions(instructions);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyCookingMethod(String cookingMethod) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingMethod";
        recipeForm.setCookingMethod(cookingMethod);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyCuisineId(String cuisineId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(cuisineId);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyNumberOfServings() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "numberOfServings";
        recipeForm.setNumberOfServings(null);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyPreparationTimeDuration() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeDuration";
        recipeForm.setPreparationTimeDuration(null);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyPreparationTimeUnitId(String preparationTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeUnitId";
        recipeForm.setPreparationTimeUnitId(preparationTimeUnitId);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyCookingTimeDuration() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeDuration";
        recipeForm.setCookingTimeDuration(null);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyCookingTimeUnitId(String cookingTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeUnitId";
        recipeForm.setCookingTimeUnitId(cookingTimeUnitId);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyPortionSizeAmount() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeAmount";
        recipeForm.setPortionSizeAmount(null);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyPortionSizeUnitId(String portionSizeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeUnitId";
        recipeForm.setPortionSizeUnitId(portionSizeUnitId);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Post_ShouldReturn_500Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInvalidItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-MENU-001";
        String fieldName = "id";
        recipeForm.setItemId("r");

        mvcResult = mockMvc.perform(post(RECIPE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidCuisineId(String cuisineId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(cuisineId);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(ints = { -1, 0 })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidNumberOfServings(Integer numberOfServings) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "numberOfServings";
        recipeForm.setNumberOfServings(numberOfServings);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidPreparationTimeDuration(Double preparationTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeDuration";
        recipeForm.setPreparationTimeDuration(preparationTimeDuration);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidPreparationTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeUnitId";
        recipeForm.setPreparationTimeUnitId("r");

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidCookingTimeDuration(Double cookingTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeDuration";
        recipeForm.setCookingTimeDuration(cookingTimeDuration);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidCookingTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeUnitId";
        recipeForm.setCookingTimeUnitId("r");

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidPortionSizeAmount(Double portionSizeAmount) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeAmount";
        recipeForm.setPortionSizeAmount(portionSizeAmount);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithInvalidPortionSizeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeUnitId";
        recipeForm.setPortionSizeUnitId("r");

        mvcResult = mockMvc.perform(post(RECIPE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Post_ShouldReturn_500Response_And_ErrorCode_RES_MENU_002_WhenRequested_WithAbsentItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-MENU-002";
        String fieldName = "id";
        String keyword = "unavailable";
        recipeForm.setItemId("3");

        mvcResult = mockMvc.perform(post(RECIPE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_Recipe_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithAbsentCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(String.valueOf(Long.MAX_VALUE));

        mvcResult = mockMvc.perform(post(RECIPE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Post_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenRequested_WithDuplicateRecipe() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "cuisineId";
        String field3Name = "itemId";
        String field1Value = recipeEntity2.getName();
        String field2Value = recipeEntity2.getCuisine().getId().toString();
        String field3Value = recipeEntity2.getItemId();
        recipeForm.setName(field1Value);
        recipeForm.setCuisineId(field2Value);
        recipeForm.setItemId(field3Value);

        mvcResult = mockMvc.perform(post(RECIPE_URI)
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
    public void test_Recipe_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdatedById_WithValidRecipeForm() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        recipeForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(RECIPE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Put_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdatedById_WithNoRecipeForm() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyName(String name) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        recipeForm.setName(name);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyItemId(String itemId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        recipeForm.setItemId(itemId);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyInstructions(String instructions) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "instructions";
        recipeForm.setInstructions(instructions);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyCookingMethod(String cookingMethod) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingMethod";
        recipeForm.setCookingMethod(cookingMethod);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyCuisineId(String cuisineId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(cuisineId);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyPreparationTimeUnitId(String preparationTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeUnitId";
        recipeForm.setPreparationTimeUnitId(preparationTimeUnitId);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyCookingTimeUnitId(String cookingTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeUnitId";
        recipeForm.setCookingTimeUnitId(cookingTimeUnitId);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithEmptyPortionSizeUnitId(String portionSizeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeUnitId";
        recipeForm.setPortionSizeUnitId(portionSizeUnitId);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Put_ShouldReturn_500Response_And_ErrorCode_RES_MENU_001_WhenUpdatedById_WithInvalidItemId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = "RES-MENU-001";
        String fieldName = "id";
        recipeForm.setItemId("r");

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidCuisineId(String cuisineId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(cuisineId);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(ints = { -1, 0 })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidNumberOfServings(Integer numberOfServings) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "numberOfServings";
        recipeForm.setNumberOfServings(numberOfServings);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPreparationTimeDuration(Double preparationTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeDuration";
        recipeForm.setPreparationTimeDuration(preparationTimeDuration);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPreparationTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeUnitId";
        recipeForm.setPreparationTimeUnitId("r");

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidCookingTimeDuration(Double cookingTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeDuration";
        recipeForm.setCookingTimeDuration(cookingTimeDuration);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidCookingTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeUnitId";
        recipeForm.setCookingTimeUnitId("r");

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPortionSizeAmount(Double portionSizeAmount) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeAmount";
        recipeForm.setPortionSizeAmount(portionSizeAmount);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPortionSizeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeUnitId";
        recipeForm.setPortionSizeUnitId("r");

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Put_ShouldReturn_500Response_And_ErrorCode_RES_MENU_002_WhenUpdatedById_WithAbsentItemId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = "RES-MENU-002";
        String fieldName = "id";
        String keyword = "unavailable";
        recipeForm.setItemId("3");

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_Recipe_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithAbsentCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        recipeForm.setCuisineId(String.valueOf(Long.MAX_VALUE));

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(recipeForm)))
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
    public void test_Recipe_Put_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenUpdatedById_WithDuplicateRecipe() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "cuisineId";
        String field3Name = "itemId";
        String field1Value = recipeEntity2.getName();
        String field2Value = recipeEntity2.getCuisine().getId().toString();
        String field3Value = recipeEntity2.getItemId();
        recipeForm.setName(field1Value);
        recipeForm.setCuisineId(field2Value);
        recipeForm.setItemId(field3Value);

        mvcResult = mockMvc.perform(put(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdatedById_WithValidRecipeAttributes() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();

        mvcResult = this.mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Patch_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdatedById_WithNoRecipeAttributes() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyName(String name) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", name));


        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyItemId(String itemId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/itemId", itemId));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }


    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyInstructions(String instructions) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/instructions", instructions));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyCookingMethod(String cookingMethod) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/cookingMethod", cookingMethod));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyCuisineId(String cuisineId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/cuisineId", cuisineId));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    /**
     * Empty checks against fields of primitive types not possible
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyPreparationTimeUnitId(String preparationTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/preparationTimeUnitId", preparationTimeUnitId));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyCookingTimeUnitId(String cookingTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/cookingTimeUnitId", cookingTimeUnitId));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdatedById_WithEmptyPortionSizeUnitId(String portionSizeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/portionSizeUnitId", portionSizeUnitId));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    /**
     * PATCH with empty fields - END
     */

    /**
     * PATCH with invalid fields - START
     */

    @Test
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidDefinitionOfRecipeAttribute() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String keyword = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @Test
    public void test_Recipe_Patch_ShouldReturn_500Response_And_ErrorCode_RES_MENU_001_WhenUpdatedById_WithInvalidItemId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = "RES-MENU-001";
        String keyword = "id";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/itemId", "r"));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(keyword));

    }

    @ParameterizedTest
    @ValueSource(strings = { "-1", "0" })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidCuisineId(String cuisineId) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, cuisineId));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    @ValueSource(ints = { -1, 0 })
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidNumberOfServings(Integer numberOfServings) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "numberOfServings";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, numberOfServings.toString()));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPreparationTimeDuration(Double preparationTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeDuration";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, preparationTimeDuration.toString()));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPreparationTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeUnitId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "r"));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidCookingTimeDuration(Double cookingTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeDuration";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, cookingTimeDuration.toString()));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidCookingTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeUnitId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "r"));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPortionSizeAmount(Double portionSizeAmount) throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeAmount";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, portionSizeAmount.toString()));


        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithInvalidPortionSizeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeUnitId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "r"));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithAbsentItemId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        String keyword = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "3"));


        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedById_WithAbsentCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, String.valueOf(Long.MAX_VALUE)));


        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Patch_ShouldReturn_409Response_And_ErrorCode_RES_COOK_004_WhenUpdatedById_WithDuplicateRecipe() throws Exception {
        MvcResult mvcResult = null;
        String id = recipeEntity1.getId().toString();
        String errorCode = CookbookErrorCode.COOK_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "cuisineId";
        String field3Name = "itemId";
        String field1Value = recipeEntity2.getName();
        String field2Value = recipeEntity2.getCuisine().getId().toString();
        String field3Value = recipeEntity2.getItemId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value),
                new PatchOperationForm("replace", "/" + field3Name, field3Value));

        mvcResult = mockMvc.perform(patch(RECIPE_URI_BY_ID, id)
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
    public void test_Recipe_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = recipeEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(RECIPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Recipe_Delete_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001__WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(RECIPE_URI_BY_ID, id))
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

        mvcResult = this.mockMvc.perform(delete(RECIPE_URI_BY_ID, id))
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

        mvcResult = this.mockMvc.perform(delete(RECIPE_URI_BY_ID, id))
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
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForAllRecipes() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));

        mvcResult = this.mockMvc.perform(get(RECIPE_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    /**
     * GET all - END
     */

    /**
     * GET all by path variable - START
     */

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_IngredientDetails_WhenRequested_ById() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;
        recipeVo1.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(recipeVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(recipeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), IngredientVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id))
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

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = recipeEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getId());
        Assertions.assertEquals(recipeVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getName());
        Assertions.assertEquals(recipeVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getDescription());
        Assertions.assertEquals(recipeVo1.getInstructions(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getInstructions());
        Assertions.assertEquals(recipeVo1.getNumberOfServings(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getNumberOfServings());
        Assertions.assertEquals(recipeVo1.getItemId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getItemId());
        Assertions.assertEquals(recipeVo1.getPreparationTime(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getPreparationTime());
        Assertions.assertEquals(recipeVo1.getCookingTime(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCookingTime());
        Assertions.assertEquals(recipeVo1.getCookingMethod(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCookingMethod());
        Assertions.assertEquals(recipeVo1.getPortionSize(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getPortionSize());
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
        recipeVo1.setItem(itemVo1);
        recipeVo1.setCuisine(cuisineVo1);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getId());
        Assertions.assertEquals(recipeVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getName());
        Assertions.assertEquals(recipeVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getDescription());
        Assertions.assertEquals(recipeVo1.getInstructions(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getInstructions());
        Assertions.assertEquals(recipeVo1.getNumberOfServings(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getNumberOfServings());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getItem() != null);
        Assertions.assertEquals(recipeVo1.getItem().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getItem().getId());
        Assertions.assertEquals(recipeVo1.getPreparationTime(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getPreparationTime());
        Assertions.assertEquals(recipeVo1.getCookingTime(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCookingTime());
        Assertions.assertEquals(recipeVo1.getCookingMethod(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCookingMethod());
        Assertions.assertEquals(recipeVo1.getPortionSize(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getPortionSize());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCuisine() != null);
        Assertions.assertEquals(recipeVo1.getCuisine().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCuisine().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo.class).getActive()));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_ByCuisineId() throws Exception {
        MvcResult mvcResult = null;

        List<RecipeVo> recipeList = Arrays.asList(recipeVo1);
        recipeVo1.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_CUISINE_ID, cuisineEntity1.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_ByEmptyCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String cuisineId = " ";
        String fieldName = "cuisineId";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_CUISINE_ID, cuisineId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_404Response_And_ErrorCode_RES_COOK_001_WhenRequested_ByInvalidCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String cuisineId = "kk";
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_CUISINE_ID, cuisineId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(cuisineId));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_404Response_And_ErrorCode_RES_COOK_001_WhenRequested_ByAbsentCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String cuisineId = String.valueOf(Long.MAX_VALUE);
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_CUISINE_ID, cuisineId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(cuisineId));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_404Response_And_ErrorCode_RES_COOK_001_WhenRequested_ByInactiveCuisineId() throws Exception {
        MvcResult mvcResult = null;
        String cuisineId = cuisineEntity3.getId().toString();
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cuisineId";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_CUISINE_ID, cuisineId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(cuisineId));
    }

    /**
     * GET all by path variable - END
     */

    /**
     * GET all by empty filters - START
     */

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyInstructionsOnly(String instructions) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("instructions", instructions))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyCookingMethodOnly(String cookingMethod) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("cookingMethod", cookingMethod))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyItemIdOnly(String itemId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("itemId", itemId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyNumberOfServingsOnly(String numberOfServings) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("numberOfServings", numberOfServings))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyPreparationTimeDurationOnly(String preparationTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("preparationTimeDuration", preparationTimeDuration))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyPreparationTimeUnitIdOnly(String preparationTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("preparationTimeUnitId", preparationTimeUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyCookingTimeDurationOnly(String cookingTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("cookingTimeDuration", cookingTimeDuration))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyCookingTimeUnitIdOnly(String cookingTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("cookingTimeUnitId", cookingTimeUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyPortionSizeAmountOnly(String portionSizeAmount) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("portionSizeAmount", portionSizeAmount))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyPortionSizeUnitIdOnly(String portionSizeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("portionSizeUnitId", portionSizeUnitId))
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
    @ValueSource(strings = { "r", "999999999999999" })
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_InvalidItemIdOnly(String itemId) throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("itemId", itemId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(ints = { -1, 0 })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidNumberOfServingsOnly(Integer numberOfServings) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("numberOfServings", numberOfServings.toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidPreparationTimeDurationOnly(Double preparationTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeDuration";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam(fieldName, preparationTimeDuration.toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r", "kg" })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidPreparationTimeUnitIdOnly(String preparationTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "preparationTimeUnitId";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam(fieldName, preparationTimeUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidCookingTimeDurationOnly(Double cookingTimeDuration) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeDuration";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam(fieldName, cookingTimeDuration.toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r", "kg" })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidCookingTimeUnitIdOnly(String cookingTimeUnitId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cookingTimeUnitId";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam(fieldName, cookingTimeUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(doubles = { -1.0d, 0.0d })
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidPortionSizeAmountOnly(Double portionSizeAmount) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "portionSizeAmount";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam(fieldName, portionSizeAmount.toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_InvalidPortionSizeUnitIdOnly() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String portionSizeUnitId = "r";
        String fieldName = "portionSizeUnitId";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam(fieldName, portionSizeUnitId))
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
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentNameOnly() throws Exception {
        MvcResult mvcResult = null;
        String name = "x";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentDescriptionOnly() throws Exception {
        MvcResult mvcResult = null;
        String description = "x";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentInstructionsOnly() throws Exception {
        MvcResult mvcResult = null;
        String instructions = "x";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("instructions", instructions))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentCookingMethodOnly() throws Exception {
        MvcResult mvcResult = null;
        String cookingMethod = "x";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("cookingMethod", cookingMethod))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentItemIdOnly() throws Exception {
        MvcResult mvcResult = null;
        String itemId = "x";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("itemId", itemId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_AbsentNumberOfServingsOnly() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String numberOfServings = String.valueOf(Long.MAX_VALUE);
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("numberOfServings", numberOfServings))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentPreparationTimeDurationAndValidPreparationTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String preparationTimeDuration = String.valueOf(Double.MAX_VALUE);
        String preparationTimeUnitId = "m";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("preparationTimeDuration", preparationTimeDuration)
                        .queryParam("preparationTimeUnitId", preparationTimeUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentCookingTimeDurationOnlyAndValidCookingTimeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String cookingTimeDuration = String.valueOf(Double.MAX_VALUE);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("cookingTimeDuration", cookingTimeDuration)
                            .queryParam("cookingTimeUnitId", "m"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentPortionSizeAmountAndValidPortionSizeUnitId() throws Exception {
        MvcResult mvcResult = null;
        String portionSizeAmount = String.valueOf(Double.MAX_VALUE);
        String portionSizeUnitId = "g";

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("portionSizeAmount", portionSizeAmount)
                        .queryParam("portionSizeUnitId", portionSizeUnitId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    /**
     * GET all by absent filters - END
     */

    /**
     * GET all by filter combination - START
     */



    /**
     * GET all by filter combination - END
     */

    // 00000000000000000000000000000000000

    /*@Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentDescriptionName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentInstructions() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("instructions", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentItemId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("itemId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentCuisineId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("cuisineId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequestedBy_AbsentCookingMethod() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER).queryParam("cookingMethod", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }


    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1));
        recipeVo1.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("description", "Recipe 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithInstructions() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1));
        recipeVo1.setCuisine(null);
        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("instructions", "Recipe 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithInstructions() throws Exception {
        MvcResult mvcResult = null;
        recipeVo2.setCuisine(null);
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo2));

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("instructions", "200111"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithItemId() throws Exception {
        MvcResult mvcResult = null;
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));
        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("itemId", "133024"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithCuisineId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);


        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("cuisineId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithCookingMethod() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>(Arrays.asList(recipeVo1, recipeVo2, recipeVo3));
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("cookingMethod", "IN"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithCuisineIdAndItemId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo1, recipeVo2, recipeVo3);
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("itemId", "133024")
                        .queryParam("cuisineId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithCookingMethodAndCuisineId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo1, recipeVo2, recipeVo3);
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("cookingMethod", "IN")
                        .queryParam("cuisineId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithCookingMethodAndItemId() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo1, recipeVo2, recipeVo3);
        recipeVo1.setCuisine(null);
        recipeVo2.setCuisine(null);
        recipeVo3.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("cookingMethod", "IN")
                        .queryParam("itemId", "133024"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithItemIdAndInstructions() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo2);
        recipeVo2.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("itemId", "133024")
                        .queryParam("instructions", "200111"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_RecipeListNaturallyOrdered_WhenRequested_ForRecipes_WithDescriptionAndInstructionsAndCookingMethod() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = Arrays.asList(recipeVo2);
        recipeVo2.setCuisine(null);

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("description", "Recipe 2")
                        .queryParam("instructions", "200111")
                        .queryParam("cookingMethod", "IN"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(recipeList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequested_ForRecipes_WithAbsent_DescriptionAndItemIdAndCookingMethod() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("description", "Recipe 1")
                        .queryParam("itemId", UUID.randomUUID().toString())
                        .queryParam("cookingMethod", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(recipeList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), RecipeVo[].class).length);
    }

    @Test
    public void test_Recipe_Get_ShouldReturn_200Response_And_EmptyRecipeList_WhenRequested_ForRecipes_WithAbsent_ItemIdAndCuisineIdAndCookingMethod() throws Exception {
        MvcResult mvcResult = null;
        List<RecipeVo> recipeList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_FILTER)
                        .queryParam("itemId", UUID.randomUUID().toString())
                        .queryParam("cuisineId", UUID.randomUUID().toString())
                        .queryParam("cookingMethod", UUID.randomUUID().toString()))
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

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id))
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

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id))
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

        mvcResult = this.mockMvc.perform(get(RECIPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    */

    @Override
    public String getSimulationBaseLocation() {
        return "simulation/menu-service";
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
