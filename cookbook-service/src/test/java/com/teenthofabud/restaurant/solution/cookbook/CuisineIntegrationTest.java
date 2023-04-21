package com.teenthofabud.restaurant.solution.cookbook;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineForm;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.CuisineVo;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.repository.CuisineRepository;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeEntity;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeForm;
import com.teenthofabud.restaurant.solution.cookbook.recipe.data.RecipeVo;
import com.teenthofabud.restaurant.solution.cookbook.recipe.repository.RecipeRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
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
public class CuisineIntegrationTest extends CookbookIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ACCOUNT_URI = "/cuisine";
    private static final String ACCOUNT_URI_BY_ID = "/cuisine/{id}";
    private static final String ACCOUNT_URI_FILTER = "/cuisine/filter";

    private CuisineRepository cuisineRepository;

    private RecipeRepository recipeRepository;

    @Autowired
    public void setCuisineRepository(CuisineRepository cuisineRepository) {
        this.cuisineRepository = cuisineRepository;
    }

    private CuisineForm cuisineForm;

    @Autowired
    public void setRecipeRepository(RecipeRepository recipeRepository) {
        this.recipeRepository = recipeRepository;
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

        cuisineForm = new CuisineForm();
        cuisineForm.setName("New Name");
        cuisineForm.setDescription("New Description");

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

        cuisineEntity1.setRecipes(new ArrayList<>(Arrays.asList(recipeEntity1)));
        cuisineEntity1 = cuisineRepository.save(cuisineEntity1);
        cuisineEntity2.setRecipes(new ArrayList<>(Arrays.asList(recipeEntity2)));
        cuisineEntity2 = cuisineRepository.save(cuisineEntity2);
        cuisineEntity3.setRecipes(new ArrayList<>(Arrays.asList(recipeEntity3)));
        cuisineEntity3 = cuisineRepository.save(cuisineEntity3);

        recipeVo1.setCuisine(cuisineVo1);
        recipeVo2.setCuisine(cuisineVo2);
        recipeVo3.setCuisine(cuisineVo3);

        cuisineVo1.setRecipes(Arrays.asList(recipeVo1));
        cuisineVo2.setRecipes(Arrays.asList(recipeVo2));
        cuisineVo3.setRecipes(Arrays.asList(recipeVo3));
    }

    @AfterEach
    private void destroy() {
        recipeEntity1.setCuisine(null);
        recipeEntity2.setCuisine(null);
        recipeEntity3.setCuisine(null);

        recipeRepository.deleteById(recipeEntity1.getId());
        recipeRepository.deleteById(recipeEntity2.getId());
        recipeRepository.deleteById(recipeEntity3.getId());

        cuisineEntity1.setRecipes(null);
        cuisineEntity2.setRecipes(null);
        cuisineEntity3.setRecipes(null);
        cuisineEntity4.setRecipes(null);

        cuisineRepository.deleteById(cuisineEntity1.getId());
        cuisineRepository.deleteById(cuisineEntity2.getId());
        cuisineRepository.deleteById(cuisineEntity3.getId());
        cuisineRepository.deleteById(cuisineEntity4.getId());
    }


    @Test
    public void test_Cuisine_Post_ShouldReturn_201Response_And_NewCuisineId_WhenPosted_WithValidCuisineForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ACCOUNT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Cuisine_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        cuisineForm.setName("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Cuisine_Post_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        cuisineForm.setDescription("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Cuisine_Post_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenPosted_WithNoCuisineForm() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
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
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForAllCuisines() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineVo> cuisineList = new LinkedList<>(Arrays.asList(cuisineVo1, cuisineVo2, cuisineVo3, cuisineVo4));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Cuisine_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Cuisine_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_EmptyCuisineList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_EmptyCuisineList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineVo> cuisineList = new ArrayList<>(Arrays.asList(cuisineVo1, cuisineVo2, cuisineVo3, cuisineVo4));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineVo> cuisineList = new ArrayList<>(Arrays.asList(cuisineVo2));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("description", "Cuisine 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineVo> cuisineList = new LinkedList<>(Arrays.asList(cuisineVo1));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine 1")
                        .queryParam("description", "Cuisine 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithNameAndDescriptionAndPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineVo> cuisineList = new LinkedList<>(Arrays.asList(cuisineVo1));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine 1")
                        .queryParam("description", "Cuisine 1")
                        .queryParam("phoneNumber", "1122334455"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_EmptyCuisineList_WhenRequested_ForCuisines_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineVo> cuisineList = new LinkedList<>();

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Cuisine_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_400Response_And_ErrorCode_RES_COOK_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = cuisineEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getId());
        Assertions.assertEquals(cuisineVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getName());
        Assertions.assertEquals(cuisineVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getDescription());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getActive()));
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getId());
        Assertions.assertEquals(cuisineVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getName());
        Assertions.assertEquals(cuisineVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getDescription());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getRecipes() != null);
        Assertions.assertEquals(cuisineVo1.getRecipes().size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getRecipes().size());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineVo.class).getActive()));
    }

    @Test
    public void test_Cuisine_Delete_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Delete_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Delete_ShouldReturn_400Response_And_ErrorCode_RES_COOK_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = cuisineEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Cuisine_Delete_ShouldReturn_404Response_And_ErrorCode_RES_COOK_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndCuisineDetails() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        cuisineForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Cuisine_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenUpdatedBy_EmptyInvalidId_AndCuisineDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Put_ShouldReturn_404Response_And_ErrorCode_RES_COOK_002_WhenUpdated_ByAbsentId_AndCuisineDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Cuisine_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_005_WhenUpdated_ByInactiveId_AndCuisineDetails() throws Exception {
        String id = cuisineEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Cuisine_Put_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdated_ById_AndNoCuisineDetails() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
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
    public void test_Cuisine_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        cuisineForm.setName("");

        mvcResult = mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Put_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        cuisineForm.setDescription("");

        mvcResult = mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(cuisineForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Cuisine_Put_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdated_ById_AndEmptyCuisineDetails() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new CuisineForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Cuisine_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndCuisineDetails() throws Exception {
        String id = cuisineEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Cuisine_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndCuisineDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, " ")
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
    public void test_Cuisine_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_002_WhenUpdated_ByInvalidId_AndCuisineDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Cuisine_Patch_ShouldReturn_404Response_And_ErrorCode_RES_COOK_002_WhenUpdated_ByAbsentId_AndCuisineDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Cuisine_Patch_ShouldReturn_422Response_And_ErrorCode_RES_COOK_003_WhenUpdated_ById_AndNoCuisineDetails() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Cuisine_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Cuisine_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Cuisine_Patch_ShouldReturn_400Response_And_ErrorCode_RES_COOK_001_WhenRequested_ById_AndInvalidDefinitionOfCuisineAttribute() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CookbookErrorCode.COOK_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
