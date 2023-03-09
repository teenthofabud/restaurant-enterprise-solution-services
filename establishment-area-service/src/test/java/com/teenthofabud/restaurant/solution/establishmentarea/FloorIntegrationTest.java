package com.teenthofabud.restaurant.solution.establishmentarea;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.EstablishmentAreaIntegrationBaseTest;
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
public class FloorIntegrationTest extends EstablishmentAreaIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String FLOOR_URI = "/floor";
    private static final String FLOOR_URI_BY_ID = "/floor/{id}";
    private static final String FLOOR_URI_FILTER = "/floor/filter";

    private FloorRepository floorRepository;
    private KitchenRepository kitchenRepository;
    private TableRepository tableRepository;

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Autowired
    public void setKitchenRepository(KitchenRepository kitchenRepository) {
        this.kitchenRepository = kitchenRepository;
    }

    @Autowired
    public void setTableRepository(TableRepository tableRepository) {
        this.tableRepository = tableRepository;
    }

    private FloorForm floorForm;
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

    private TableForm tableForm;
    private TableVo tableVo1;
    private TableVo tableVo2;
    private TableVo tableVo3;
    private TableVo tableVo4;
    private TableEntity tableEntity1;
    private TableEntity tableEntity2;
    private TableEntity tableEntity3;
    private TableEntity tableEntity4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        floorForm = new FloorForm();
        floorForm.setFlrName("New Floor");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/flrName", "patched first name"));

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

        /*patches = Arrays.asList(
                new PatchOperationForm("replace", "/kitchenName", "patched first name"),
                new PatchOperationForm("replace", "/description", "patched last name"));*/

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
        kitchenEntity3.setActive(Boolean.TRUE);
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

        tableForm = new TableForm();
        tableForm.setTableName("New Table");
        tableForm.setDescription("New Table Description");
        tableForm.setCapacity(12);
        tableForm.setFloorId(floorEntity1.getFlrId().toString());

        tableEntity1 = new TableEntity();
        tableEntity1.setTableName("Table 1 Name");
        tableEntity1.setDescription("Table 1 Description");
        tableEntity1.setCapacity(1);
        tableEntity1.setActive(Boolean.TRUE);
        tableEntity1.setFloor(floorEntity1);

        tableEntity1 = tableRepository.save(tableEntity1);

        tableVo1 = new TableVo();
        tableVo1.setTableId(tableEntity1.getTableId().toString());
        tableVo1.setTableName(tableEntity1.getTableName());
        tableVo1.setDescription(tableEntity1.getDescription());
        tableVo1.setFloorId(tableEntity1.getFloor().getFlrId().toString());

        tableEntity2 = new TableEntity();
        tableEntity2.setTableName("Table 2 Name");
        tableEntity2.setDescription("Table 2 Description");
        tableEntity2.setCapacity(2);
        tableEntity2.setActive(Boolean.TRUE);
        tableEntity2.setFloor(floorEntity2);

        tableEntity2 = tableRepository.save(tableEntity2);

        tableVo2 = new TableVo();
        tableVo2.setTableId(tableEntity2.getTableId().toString());
        tableVo2.setTableName(tableEntity2.getTableName());
        tableVo2.setDescription(tableEntity2.getDescription());
        tableVo2.setFloorId(tableEntity2.getFloor().getFlrId().toString());

        tableEntity3 = new TableEntity();
        tableEntity3.setTableName("Table 3 Name");
        tableEntity3.setDescription("Table 3 Description");
        tableEntity3.setCapacity(3);
        tableEntity3.setActive(Boolean.TRUE);
        tableEntity3.setFloor(floorEntity1);

        tableEntity3 = tableRepository.save(tableEntity3);

        tableVo3 = new TableVo();
        tableVo3.setTableId(tableEntity3.getTableId().toString());
        tableVo3.setTableName(tableEntity3.getTableName());
        tableVo3.setDescription(tableEntity3.getDescription());
        tableVo3.setFloorId(tableEntity3.getFloor().getFlrId().toString());

        tableEntity4 = new TableEntity();
        tableEntity4.setTableName("Table 4 Name");
        tableEntity4.setDescription("Table 4 Description");
        tableEntity4.setCapacity(4);
        tableEntity4.setActive(Boolean.TRUE);
        tableEntity4.setFloor(floorEntity1);

        tableEntity4 = tableRepository.save(tableEntity4);

        tableVo4 = new TableVo();
        tableVo4.setTableId(tableEntity4.getTableId().toString());
        tableVo4.setTableName(tableEntity4.getTableName());
        tableVo4.setDescription(tableEntity4.getDescription());
        tableVo4.setFloorId(tableEntity4.getFloor().getFlrId().toString());

        floorEntity1.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity1)));
        floorEntity1 = floorRepository.save(floorEntity1);
        floorEntity2.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity2)));
        floorEntity2 = floorRepository.save(floorEntity2);
        floorEntity3.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity3)));
        floorEntity3 = floorRepository.save(floorEntity3);
        floorEntity4.setKitchen(new ArrayList<KitchenEntity>(List.of(kitchenEntity4)));
        floorEntity4 = floorRepository.save(floorEntity4);

        floorEntity1.setTable(new ArrayList<TableEntity>(List.of(tableEntity1)));
        floorEntity1 = floorRepository.save(floorEntity1);
        floorEntity2.setTable(new ArrayList<TableEntity>(List.of(tableEntity2)));
        floorEntity2 = floorRepository.save(floorEntity2);
        floorEntity3.setTable(new ArrayList<TableEntity>(List.of(tableEntity3)));
        floorEntity3 = floorRepository.save(floorEntity3);
        floorEntity4.setTable(new ArrayList<TableEntity>(List.of(tableEntity4)));
        floorEntity4 = floorRepository.save(floorEntity4);

        kitchenVo1.setFloorVo(floorVo1);
        kitchenVo2.setFloorVo(floorVo2);
        kitchenVo3.setFloorVo(floorVo3);
        kitchenVo4.setFloorVo(floorVo4);

        tableVo1.setFloorVo(floorVo1);
        tableVo2.setFloorVo(floorVo2);
        tableVo3.setFloorVo(floorVo3);
        tableVo4.setFloorVo(floorVo4);

        floorVo1.setKitchens(Arrays.asList(kitchenVo1));
        floorVo2.setKitchens(Arrays.asList(kitchenVo2));
        floorVo3.setKitchens(Arrays.asList(kitchenVo3));
        floorVo4.setKitchens(Arrays.asList(kitchenVo4));

        floorVo1.setTables(Arrays.asList(tableVo1));
        floorVo2.setTables(Arrays.asList(tableVo2));
        floorVo3.setTables(Arrays.asList(tableVo3));
        floorVo4.setTables(Arrays.asList(tableVo4));
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

        tableRepository.deleteById(tableEntity1.getTableId());
        tableRepository.deleteById(tableEntity2.getTableId());
        tableRepository.deleteById(tableEntity3.getTableId());
        tableRepository.deleteById(tableEntity4.getTableId());

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

        tableEntity1.setFloor(null);
        tableEntity2.setFloor(null);
        tableEntity3.setFloor(null);
        tableEntity4.setFloor(null);
    }


    @Test
    public void test_Floor_Post_ShouldReturn_201Response_And_NewFloorId_WhenPosted_WithValidFloorForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(FLOOR_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Floor_Post_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001_WhenRequested_WithEmptyFloorName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "flrName";
        floorForm.setFlrName("");

        mvcResult = mockMvc.perform(post(FLOOR_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Post_ShouldReturn_422Response_And_ErrorCode_RES_EAREA_003_WhenPosted_WithNoFloorForm() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(FLOOR_URI)
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
    public void test_Floor_Get_ShouldReturn_200Response_And_FloorListNaturallyOrdered_WhenRequested_ForAllFloors() throws Exception {
        MvcResult mvcResult = null;
        Set<FloorVo> floorList = new TreeSet<>(Arrays.asList(floorVo1, floorVo2, floorVo3, floorVo4));

        mvcResult = this.mockMvc.perform(get(FLOOR_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(floorList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Floor_Get_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001_WhenRequestedBy_EmptyFloorNameOnly(String floorName) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(FLOOR_URI_FILTER).queryParam("floorName", floorName))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Get_ShouldReturn_200Response_And_EmptyFloorList_WhenRequestedBy_AbsentFirstName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(FLOOR_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo[].class).length);
    }

    @Test
    public void test_Floor_Get_ShouldReturn_200Response_And_FloorListNaturallyOrdered_WhenRequested_ForFloors_WithFirstName() throws Exception {
        MvcResult mvcResult = null;
        List<FloorVo> floorList = new ArrayList<>(Arrays.asList(floorVo1, floorVo2, floorVo3, floorVo4));

        mvcResult = this.mockMvc.perform(get(FLOOR_URI_FILTER)
                        .queryParam("name", "Floor"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(floorList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo[].class).length);
    }

    @Test
    public void test_Floor_Get_ShouldReturn_200Response_And_FloorDetails_WhenRequested_ById() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        floorVo1.setKitchens(null);
        floorVo1.setTables(null);

        mvcResult = this.mockMvc.perform(get(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(floorVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(floorVo1.getFlrId(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getFlrId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Floor_Get_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Get_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = floorEntity2.getFlrId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Floor_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(FLOOR_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(floorVo1.getFlrId(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getFlrId());
        Assertions.assertEquals(floorVo1.getFlrName(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getFlrName());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getActive()));
    }

    @Test
    public void test_Floor_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        mvcResult = this.mockMvc.perform(get(FLOOR_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(floorVo1.getFlrId(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getFlrId());
        Assertions.assertEquals(floorVo1.getFlrName(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getFlrName());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getKitchens() != null);
        Assertions.assertEquals(floorVo1.getKitchens().size(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getKitchens().size());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getTables() != null);
        Assertions.assertEquals(floorVo1.getTables().size(), om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getTables().size());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), FloorVo.class).getActive()));
    }

    @Test
    public void test_Floor_Delete_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(FLOOR_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Delete_ShouldReturn_422Response_And_ErrorCode_RES_EAREA_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Delete_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = floorEntity4.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Floor_Delete_ShouldReturn_404Response_And_ErrorCode_RES_EAREA_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndFloorDetails() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        floorForm.setFlrName("Ground");

        mvcResult = this.mockMvc.perform(put(FLOOR_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Floor_Put_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001_WhenUpdatedBy_EmptyInvalidId_AndFloorDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(FLOOR_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Put_ShouldReturn_404Response_And_ErrorCode_RES_EAREA_002_WhenUpdated_ByAbsentId_AndFloorDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(FLOOR_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Floor_Put_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_005_WhenUpdated_ByInactiveId_AndFloorDetails() throws Exception {
        String id = floorEntity4.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(FLOOR_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Floor_Put_ShouldReturn_422Response_And_ErrorCode_RES_EAREA_003_WhenUpdated_ById_AndNoFloorDetails() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(FLOOR_URI_BY_ID, id)
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
    public void test_Floor_Put_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001_WhenRequested_ById_AndInvalidFloorName() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "flrName";
        floorForm.setFlrName("");

        mvcResult = mockMvc.perform(put(FLOOR_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Floor_Put_ShouldReturn_422Response_And_ErrorCode_RES_EAREA_003_WhenUpdated_ById_AndEmptyFloorDetails() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(FLOOR_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new FloorForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Floor_Put_ShouldReturn_409Response_And_ErrorCode_RES_EAREA_004_WhenUpdated_ById_AndDuplicateFloorDetails() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String field1Name = "flrName";
        floorForm.setFlrName(floorEntity2.getFlrName());

        mvcResult = mockMvc.perform(put(FLOOR_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(floorForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Floor_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndFloorDetails() throws Exception {
        String id = floorEntity4.getFlrId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(FLOOR_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Floor_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndFloorDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(FLOOR_URI_BY_ID, " ")
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
    public void test_Floor_Patch_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_002_WhenUpdated_ByInvalidId_AndFloorDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(FLOOR_URI_BY_ID, id)
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
    public void test_Floor_Patch_ShouldReturn_404Response_And_ErrorCode_RES_EAREA_002_WhenUpdated_ByAbsentId_AndFloorDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(FLOOR_URI_BY_ID, id)
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
    public void test_Floor_Patch_ShouldReturn_409Response_And_ErrorCode_RES_EAREA_002_WhenUpdated_ById_AndDuplicateFloorDetails() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String fieldName = "flrName";
        String fieldValue = floorEntity3.getFlrName();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/flrName", fieldValue));


        mvcResult = this.mockMvc.perform(patch(FLOOR_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_Floor_Patch_ShouldReturn_422Response_And_ErrorCode_RES_EAREA_003_WhenUpdated_ById_AndNoFloorDetails() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(FLOOR_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Floor_Patch_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(FLOOR_URI_BY_ID, id)
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
    public void test_Floor_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/flrName", " "));

        mvcResult = mockMvc.perform(patch(FLOOR_URI_BY_ID, id)
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
    public void test_Floor_Patch_ShouldReturn_400Response_And_ErrorCode_RES_EAREA_001_WhenRequested_ById_AndInvalidDefinitionOfFloorAttribute() throws Exception {
        String id = floorEntity1.getFlrId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(FLOOR_URI_BY_ID, id)
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
