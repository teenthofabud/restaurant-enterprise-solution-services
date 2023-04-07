package com.teenthofabud.restaurant.solution.establishmentarea;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorForm;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenForm;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.data.KitchenVo;
import com.teenthofabud.restaurant.solution.establishmentarea.kitchen.repository.KitchenRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableForm;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableVo;
import com.teenthofabud.restaurant.solution.establishmentarea.table.repository.TableRepository;
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
public class KitchenIntegrationTest extends EstablishmentAreaIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String KITCHEN_URI = "/kitchen";
    private static final String KITCHEN_URI_BY_ID = "/kitchen/{id}";
    private static final String KITCHEN_URI_FILTER = "/kitchen/filter";
    private static final String KITCHEN_URI_BY_FLOOR_ID = "/kitchen/floorid/{floorId}";

    private FloorRepository floorRepository;
    private KitchenRepository kitchenRepository;

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Autowired
    public void setKitchenRepository(KitchenRepository kitchenRepository) {
        this.kitchenRepository = kitchenRepository;
    }

    private FloorVo floorVo1;
    private FloorVo floorVo2;
    private FloorVo floorVo3;
    private FloorVo floorVo4;
    private FloorEntity floorEntity1;
    private FloorEntity floorEntity2;
    private FloorEntity floorEntity3;
    private FloorEntity floorEntity4;

    private KitchenForm kitchenForm;
    private KitchenVo kitchenVo1;
    private KitchenVo kitchenVo2;
    private KitchenVo kitchenVo3;
    private KitchenVo kitchenVo4;
    private KitchenEntity kitchenEntity1;
    private KitchenEntity kitchenEntity2;
    private KitchenEntity kitchenEntity3;
    private KitchenEntity kitchenEntity4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        floorEntity1 = new FloorEntity();
        floorEntity1.setFlrName("Floor 1 Name");
        floorEntity1.setActive(Boolean.TRUE);

        floorEntity1 = floorRepository.save(floorEntity1);

        floorVo1 = new FloorVo();
        floorVo1.setFlrId(floorEntity1.getFlrId().toString());
        floorVo1.setFlrName(floorEntity1.getFlrName());

        floorEntity2 = new FloorEntity();
        floorEntity2.setFlrName("Floor 2 Name");
        floorEntity2.setActive(Boolean.TRUE);

        floorEntity2 = floorRepository.save(floorEntity2);

        floorVo2 = new FloorVo();
        floorVo2.setFlrId(floorEntity2.getFlrId().toString());
        floorVo2.setFlrName(floorEntity2.getFlrName());

        floorEntity3 = new FloorEntity();
        floorEntity3.setFlrName("Floor 3 Name");
        floorEntity3.setActive(Boolean.FALSE);

        floorEntity3 = floorRepository.save(floorEntity3);

        floorVo3 = new FloorVo();
        floorVo3.setFlrId(floorEntity3.getFlrId().toString());
        floorVo3.setFlrName(floorEntity3.getFlrName());

        floorEntity4 = new FloorEntity();
        floorEntity4.setFlrName("Floor 4 Name");
        floorEntity4.setActive(Boolean.FALSE);

        floorEntity4 = floorRepository.save(floorEntity4);

        floorVo4 = new FloorVo();
        floorVo4.setFlrId(floorEntity4.getFlrId().toString());
        floorVo4.setFlrName(floorEntity4.getFlrName());

        kitchenForm = new KitchenForm();
        kitchenForm.setKitchenName("New Kitchen");
        kitchenForm.setFloorId(floorEntity1.getFlrId().toString());
        kitchenForm.setDescription("New Kitchen Description");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/kitchenName", "patched kitchen name"),
                new PatchOperationForm("replace", "/description", "patched description"));

        kitchenEntity1 = new KitchenEntity();
        kitchenEntity1.setKitchenName("Kitchen 1 Name");
        kitchenEntity1.setDescription("Kitchen 1 Description");
        kitchenEntity1.setActive(Boolean.TRUE);
        kitchenEntity1.setFloor(floorEntity1);

        kitchenEntity1 = kitchenRepository.save(kitchenEntity1);

        kitchenVo1 = new KitchenVo();
        kitchenVo1.setKitchenId(kitchenEntity1.getKitchenId().toString());
        kitchenVo1.setKitchenName(kitchenEntity1.getKitchenName());
        kitchenVo1.setDescription(kitchenEntity1.getDescription());
        kitchenVo1.setFloorId(kitchenEntity1.getFloor().getFlrId().toString());

        kitchenEntity2 = new KitchenEntity();
        kitchenEntity2.setKitchenName("Kitchen 2 Name");
        kitchenEntity2.setDescription("Kitchen 2 Description");
        kitchenEntity2.setActive(Boolean.TRUE);
        kitchenEntity2.setFloor(floorEntity2);

        kitchenEntity2 = kitchenRepository.save(kitchenEntity2);

        kitchenVo2 = new KitchenVo();
        kitchenVo2.setKitchenId(kitchenEntity2.getKitchenId().toString());
        kitchenVo2.setKitchenName(kitchenEntity2.getKitchenName());
        kitchenVo2.setDescription(kitchenEntity2.getDescription());
        kitchenVo2.setFloorId(kitchenEntity2.getFloor().getFlrId().toString());

        kitchenEntity3 = new KitchenEntity();
        kitchenEntity3.setKitchenName("Kitchen 3 Name");
        kitchenEntity3.setDescription("Kitchen 3 Description");
        kitchenEntity3.setActive(Boolean.FALSE);
        kitchenEntity3.setFloor(floorEntity3);

        kitchenEntity3 = kitchenRepository.save(kitchenEntity3);

        kitchenVo3 = new KitchenVo();
        kitchenVo3.setKitchenId(kitchenEntity3.getKitchenId().toString());
        kitchenVo3.setKitchenName(kitchenEntity3.getKitchenName());
        kitchenVo3.setDescription(kitchenEntity3.getDescription());
        kitchenVo3.setFloorId(kitchenEntity3.getFloor().getFlrId().toString());

        kitchenEntity4 = new KitchenEntity();
        kitchenEntity4.setKitchenName("Kitchen 4 Name");
        kitchenEntity4.setDescription("Kitchen 4 Description");
        kitchenEntity4.setActive(Boolean.TRUE);
        kitchenEntity4.setFloor(floorEntity3);

        kitchenEntity4 = kitchenRepository.save(kitchenEntity4);

        kitchenVo4 = new KitchenVo();
        kitchenVo4.setKitchenId(kitchenEntity4.getKitchenId().toString());
        kitchenVo4.setKitchenName(kitchenEntity4.getKitchenName());
        kitchenVo4.setDescription(kitchenEntity4.getDescription());
        kitchenVo4.setFloorId(kitchenEntity4.getFloor().getFlrId().toString());

        floorEntity1.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity1)));
        floorEntity1 = floorRepository.save(floorEntity1);
        floorEntity2.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity2)));
        floorEntity2 = floorRepository.save(floorEntity2);
        floorEntity3.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity3)));
        floorEntity3 = floorRepository.save(floorEntity3);
        floorEntity4.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity4)));
        floorEntity4 = floorRepository.save(floorEntity4);

        kitchenVo1.setFloorVo(floorVo1);
        kitchenVo2.setFloorVo(floorVo2);
        kitchenVo3.setFloorVo(floorVo3);
        kitchenVo4.setFloorVo(floorVo4);

        floorVo1.setKitchens(Arrays.asList(kitchenVo1));
        floorVo2.setKitchens(Arrays.asList(kitchenVo2));
        floorVo3.setKitchens(Arrays.asList(kitchenVo3));
        floorVo4.setKitchens(Arrays.asList(kitchenVo4));
    }

    @AfterEach
    private void destroy() {

        floorRepository.deleteById(floorEntity1.getFlrId());
        floorRepository.deleteById(floorEntity2.getFlrId());
        floorRepository.deleteById(floorEntity3.getFlrId());
        floorRepository.deleteById(floorEntity4.getFlrId());

        kitchenRepository.deleteById(kitchenEntity1.getKitchenId());
        kitchenRepository.deleteById(kitchenEntity2.getKitchenId());
        kitchenRepository.deleteById(kitchenEntity3.getKitchenId());
        kitchenRepository.deleteById(kitchenEntity4.getKitchenId());

        floorEntity1.setKitchen(null);
        floorEntity2.setKitchen(null);
        floorEntity3.setKitchen(null);
        floorEntity4.setKitchen(null);

        floorEntity1.setTable(null);
        floorEntity2.setTable(null);
        floorEntity3.setTable(null);
        floorEntity4.setTable(null);

        kitchenEntity1.setFloor(null);
        kitchenEntity2.setFloor(null);
        kitchenEntity3.setFloor(null);
        kitchenEntity4.setFloor(null);
    }

    @Test
    public void test_Kitchen_Post_ShouldReturn_201Response_And_NewKitchenId_WhenPosted_WithValidKitchenForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(KITCHEN_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Kitchen_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyKitchenName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "kitchenName";
        kitchenForm.setKitchenName("");

        mvcResult = mockMvc.perform(post(KITCHEN_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Kitchen_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        kitchenForm.setDescription("");

        mvcResult = mockMvc.perform(post(KITCHEN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Kitchen_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        kitchenForm.setFloorId("");

        mvcResult = mockMvc.perform(post(KITCHEN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Kitchen_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithInvalidFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        kitchenForm.setFloorId("r");

        mvcResult = mockMvc.perform(post(KITCHEN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Kitchen_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithAbsentFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        kitchenForm.setFloorId("99999");

        mvcResult = mockMvc.perform(post(KITCHEN_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Kitchen_Post_ShouldReturn_409Response_And_ErrorCode_RES_CUST_004_WhenRequested_WithDuplicateKitchen() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String field1Name = "kitchenName";
        String field2Name = "floorId";
        String field1Value = kitchenEntity1.getKitchenName();
        String field2Value = kitchenEntity1.getFloor().getFlrId().toString();
        kitchenForm.setKitchenName(field1Value);
        kitchenForm.setFloorId(field2Value);

        mvcResult = mockMvc.perform(post(KITCHEN_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
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
    public void test_Kitchen_Post_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenPosted_WithNoKitchenForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(KITCHEN_URI)
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
    public void test_Kitchen_Get_ShouldReturn_200Response_And_KitchenListNaturallyOrdered_WhenRequested_ForAllKitchens() throws Exception {
        MvcResult mvcResult = null;
        List<KitchenVo> kitchenList = new ArrayList<>(Arrays.asList(kitchenVo1, kitchenVo2, kitchenVo3, kitchenVo4));

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(kitchenList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo[].class).length);
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_KitchenListNaturallyOrdered_WhenRequested_ForKitchens_ByFloorId() throws Exception {
        MvcResult mvcResult = null;

        List<KitchenVo> kitchenList = Arrays.asList(kitchenVo1);
        kitchenVo1.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_FLOOR_ID, floorEntity1.getFlrId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(kitchenList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(kitchenList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_KitchenListNaturallyOrdered_WhenRequested_ForKitchens_ByEmptyFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_FLOOR_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_404Response_And_ErrorCode_RES_CUST_001_WhenRequested_ByAbsentFloorId() throws Exception {
        MvcResult mvcResult = null;
        String floorId = "kk";
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_FLOOR_ID, floorId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(floorId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Kitchen_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyKitchenLine1Only(String kitchenName) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_FILTER).queryParam("kitchenName", kitchenName))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_EmptyKitchenList_WhenRequestedBy_AbsentKitchenName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_FILTER).queryParam("kitchenName", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo[].class).length);
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_KitchenListNaturallyOrdered_WhenRequested_ForKitchens_WithKitchenName() throws Exception {
        MvcResult mvcResult = null;
        List<KitchenVo> kitchenList = new ArrayList<>(Arrays.asList(kitchenVo1));
        kitchenVo1.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_FILTER)
                        .queryParam("kitchenName", "Kitchen 1 Name"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(kitchenList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(kitchenList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_KitchenListNaturallyOrdered_WhenRequested_ForKitchens_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<KitchenVo> kitchenList = new ArrayList<>(Arrays.asList(kitchenVo1));
        kitchenVo1.setFloorVo(null);
        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_FILTER)
                        .queryParam("description", "Kitchen 1 Description"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(kitchenList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(kitchenList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_KitchenListNaturallyOrdered_WhenRequested_ForKitchens_WithKitchenNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<KitchenVo> kitchenList = Arrays.asList(kitchenVo2);
        kitchenVo2.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_FILTER)
                        .queryParam("kitchenName", "Kitchen 2 Name")
                        .queryParam("description", "Kitchen 2 Description"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(kitchenList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(kitchenList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_KitchenDetails_WhenRequested_ById() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        kitchenVo1.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(kitchenVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(kitchenVo1.getKitchenId(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getKitchenId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Kitchen_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = kitchenEntity2.getKitchenId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(kitchenVo1.getKitchenId(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getKitchenId());
        Assertions.assertEquals(kitchenVo1.getKitchenName(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getKitchenName());
        Assertions.assertEquals(kitchenVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getDescription());
        Assertions.assertEquals(kitchenVo1.getFloorId(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getFloorId());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getActive()));
    }

    @Test
    public void test_Kitchen_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(KITCHEN_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(kitchenVo1.getKitchenId(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getKitchenId());
        Assertions.assertEquals(kitchenVo1.getKitchenName(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getKitchenName());
        Assertions.assertEquals(kitchenVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getDescription());
        Assertions.assertEquals(kitchenVo1.getFloorVo().getFlrId(), om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getFloorVo().getFlrId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), KitchenVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Kitchen_Delete_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001__WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Delete_ShouldReturn_400Response_And_ErrorCode_RES_CUST_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = kitchenEntity3.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Kitchen_Delete_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndKitchenDetails() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        kitchenForm.setKitchenName("Ferran");
        kitchenForm.setDescription("Description");
        kitchenForm.setFloorId(floorEntity1.getFlrId().toString());

        mvcResult = this.mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Kitchen_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenUpdatedBy_EmptyInvalidId_AndKitchenDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Put_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByAbsentId_AndKitchenDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Kitchen_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_005_WhenUpdated_ByInactiveId_AndKitchenDetails() throws Exception {
        String id = kitchenEntity3.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Kitchen_Put_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndNoKitchenDetails() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndEmptyKitchenName(String kitchenName) throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "kitchenName";
        kitchenForm.setKitchenName(kitchenName);

        mvcResult = mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Kitchen_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndEmptyInvalidFloorId(String floorId) throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        kitchenForm.setFloorId(floorId);

        mvcResult = mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999" })
    public void test_Kitchen_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndAbsentFloorId(String floorId) throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        kitchenForm.setFloorId(floorId);

        mvcResult = mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(kitchenForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Kitchen_Put_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndEmptyKitchenDetails() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new KitchenForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Kitchen_Put_ShouldReturn_409Response_And_ErrorCode_RES_CUST_004_WhenUpdated_ById_AndDuplicateKitchenDetails() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String field1Name = "kitchenName";
        String field2Name = "floorId";
        String field1Value = kitchenEntity2.getKitchenName();
        String field2Value = kitchenEntity2.getFloor().getFlrId().toString();
        kitchenForm.setKitchenName(field1Value);
        kitchenForm.setFloorId(field2Value);

        mvcResult = mockMvc.perform(put(KITCHEN_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(kitchenForm)))
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
    public void test_Kitchen_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndKitchenDetails() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Kitchen_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndKitchenDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(KITCHEN_URI_BY_ID, " ")
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
    public void test_Kitchen_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByInvalidId_AndKitchenDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Patch_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByAbsentId_AndKitchenDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Patch_ShouldReturn_409Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ById_AndDuplicateKitchenDetails() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String field1Name = "kitchenName";
        String field2Name = "floorId";
        String field1Value = kitchenEntity2.getKitchenName();
        String field2Value = kitchenEntity2.getFloor().getFlrId().toString();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value));


        mvcResult = this.mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Patch_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndNoKitchenDetails() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(KITCHEN_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Kitchen_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/kitchenName", " "));

        mvcResult = mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyFloorId(String floorId) throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/floorId", floorId));

        mvcResult = mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidFloorId(String floorId) throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, floorId));

        mvcResult = mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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
    public void test_Kitchen_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidDefinitionOfKitchenAttribute() throws Exception {
        String id = kitchenEntity1.getKitchenId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(KITCHEN_URI_BY_ID, id)
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

