package com.teenthofabud.restaurant.solution.cookbook;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.data.CuisineEntity;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineRequest;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.core.ports.driver.dto.CuisineResponse;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.adapters.driven.repository.CuisineJPARepository;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.integration.menu.data.ItemVo;
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
    //private static final String ACCOUNT_URI_FILTER = "/cuisine/filter";

    private CuisineJPARepository cuisineRepository;

    private int integrationServicePort;

    @Value("${cookbook.integration.service.port}")
    public void setIntegrationServicePort(int integrationServicePort) {
        this.integrationServicePort = integrationServicePort;
    }

    @Autowired
    public void setCuisineRepository(CuisineJPARepository cuisineRepository) {
        this.cuisineRepository = cuisineRepository;
    }

    private CuisineRequest cuisineRequest;

    private ItemVo itemVo1;
    private ItemVo itemVo2;
    private ItemVo itemVo3;
    private ItemVo itemVo4;

    private CuisineResponse cuisineResponse1;
    private CuisineResponse cuisineResponse2;
    private CuisineResponse cuisineResponse3;
    private CuisineResponse cuisineResponse4;
    private CuisineEntity cuisineEntity1;
    private CuisineEntity cuisineEntity2;
    private CuisineEntity cuisineEntity3;
    private CuisineEntity cuisineEntity4;


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

        cuisineRequest = new CuisineRequest();
        cuisineRequest.setName("New Name");
        cuisineRequest.setDescription("New Description");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"));


        cuisineEntity1 = new CuisineEntity();
        cuisineEntity1.setName("Cuisine 1 Name");
        cuisineEntity1.setDescription("Cuisine 1 Description");
        cuisineEntity1.setActive(Boolean.TRUE);

        cuisineEntity1 = cuisineRepository.save(cuisineEntity1);

        cuisineResponse1 = new CuisineResponse();
        cuisineResponse1.setId(cuisineEntity1.getId().toString());
        cuisineResponse1.setName(cuisineEntity1.getName());
        cuisineResponse1.setDescription(cuisineEntity1.getDescription());

        cuisineEntity2 = new CuisineEntity();
        cuisineEntity2.setName("Cuisine 2 Name");
        cuisineEntity2.setDescription("Cuisine 2 Description");
        cuisineEntity2.setActive(Boolean.TRUE);

        cuisineEntity2 = cuisineRepository.save(cuisineEntity2);

        cuisineResponse2 = new CuisineResponse();
        cuisineResponse2.setId(cuisineEntity2.getId().toString());
        cuisineResponse2.setName(cuisineEntity2.getName());
        cuisineResponse2.setDescription(cuisineEntity2.getDescription());

        cuisineEntity3 = new CuisineEntity();
        cuisineEntity3.setName("Cuisine 3 Name");
        cuisineEntity3.setDescription("Cuisine 3 Description");
        cuisineEntity3.setActive(Boolean.FALSE);

        cuisineEntity3 = cuisineRepository.save(cuisineEntity3);

        cuisineResponse3 = new CuisineResponse();
        cuisineResponse3.setId(cuisineEntity3.getId().toString());
        cuisineResponse3.setName(cuisineEntity3.getName());
        cuisineResponse3.setDescription(cuisineEntity3.getDescription());

        cuisineEntity4 = new CuisineEntity();
        cuisineEntity4.setName("Cuisine 4 Name");
        cuisineEntity4.setDescription("Cuisine 4 Description");
        cuisineEntity4.setActive(Boolean.FALSE);

        cuisineEntity4 = cuisineRepository.save(cuisineEntity4);

        cuisineResponse4 = new CuisineResponse();
        cuisineResponse4.setId(cuisineEntity4.getId().toString());
        cuisineResponse4.setName(cuisineEntity4.getName());
        cuisineResponse4.setDescription(cuisineEntity4.getDescription());

        cuisineEntity1 = cuisineRepository.save(cuisineEntity1);
        cuisineEntity2 = cuisineRepository.save(cuisineEntity2);
        cuisineEntity3 = cuisineRepository.save(cuisineEntity3);
    }

    @AfterEach
    private void destroy() {
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
                .content(om.writeValueAsString(cuisineRequest)))
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
        cuisineRequest.setName("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(cuisineRequest)))
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
        cuisineRequest.setDescription("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(cuisineRequest)))
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
    public void test_Cuisine_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndCuisineDetails() throws Exception {
        String id = cuisineEntity1.getId().toString();
        MvcResult mvcResult = null;
        cuisineRequest.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(cuisineRequest)))
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
                        .content(om.writeValueAsString(cuisineRequest)))
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
                        .content(om.writeValueAsString(cuisineRequest)))
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
                        .content(om.writeValueAsString(cuisineRequest)))
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
        cuisineRequest.setName("");

        mvcResult = mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(cuisineRequest)))
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
        cuisineRequest.setDescription("");

        mvcResult = mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(cuisineRequest)))
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
                        .content(om.writeValueAsString(new CuisineRequest())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }


   /* @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForAllCuisines() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineResponse> cuisineList = new LinkedList<>(Arrays.asList(cuisineResponse1, cuisineResponse2, cuisineResponse3, cuisineResponse4));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }*/

    /*@ParameterizedTest
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
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_EmptyCuisineList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineResponse> cuisineList = new ArrayList<>(Arrays.asList(cuisineResponse1, cuisineResponse2, cuisineResponse3, cuisineResponse4));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineResponse> cuisineList = new ArrayList<>(Arrays.asList(cuisineResponse2));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("description", "Cuisine 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineResponse> cuisineList = new LinkedList<>(Arrays.asList(cuisineResponse1));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine 1")
                        .queryParam("description", "Cuisine 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_CuisineListNaturallyOrdered_WhenRequested_ForCuisines_WithNameAndDescriptionAndPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineResponse> cuisineList = new LinkedList<>(Arrays.asList(cuisineResponse1));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine 1")
                        .queryParam("description", "Cuisine 1")
                        .queryParam("phoneNumber", "1122334455"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }

    @Test
    public void test_Cuisine_Get_ShouldReturn_200Response_And_EmptyCuisineList_WhenRequested_ForCuisines_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<CuisineResponse> cuisineList = new LinkedList<>();

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("name", "Cuisine 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(cuisineList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse[].class).length);
    }*/

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

    /*@Test
    public void test_Cuisine_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = cuisineEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }*/

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
        Assertions.assertEquals(cuisineResponse1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getId());
        Assertions.assertEquals(cuisineResponse1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getName());
        Assertions.assertEquals(cuisineResponse1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getDescription());
        //Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getCreatedBy()));
        //Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getModifiedBy()));
        //Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getCreatedOn()));
        //Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getModifiedOn()));
        //Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getActive()));
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
        Assertions.assertEquals(cuisineResponse1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getId());
        Assertions.assertEquals(cuisineResponse1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getName());
        Assertions.assertEquals(cuisineResponse1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getDescription());
        //Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getRecipes() != null);
        //Assertions.assertEquals(cuisineResponse1.getRecipes().size(), om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getRecipes().size());
        //Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getCreatedBy()));
        //Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getModifiedBy()));
        //Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getCreatedOn()));
        //Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getModifiedOn()));
        //Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), CuisineResponse.class).getActive()));
    }

    /*@Test
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

    }*/

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
