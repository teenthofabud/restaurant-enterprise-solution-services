package com.teenthofabud.restaurant.solution.establishmentarea;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorVo;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class TableIntegrationTest extends EstablishmentAreaIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String TABLE_URI = "/table";
    private static final String TABLE_URI_BY_ID = "/table/{id}";
    private static final String TABLE_URI_FILTER = "/table/filter";
    private static final String TABLE_URI_BY_FLOOR_ID = "/table/floorid/{floorId}";

    private FloorRepository floorRepository;
    private TableRepository tableRepository;

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Autowired
    public void setTableRepository(TableRepository tableRepository) {
        this.tableRepository = tableRepository;
    }

    private FloorVo floorVo1;
    private FloorVo floorVo2;
    private FloorVo floorVo3;
    private FloorVo floorVo4;
    private FloorEntity floorEntity1;
    private FloorEntity floorEntity2;
    private FloorEntity floorEntity3;
    private FloorEntity floorEntity4;

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

        tableForm = new TableForm();
        tableForm.setTableName("New Table");
        tableForm.setDescription("New Table Description");
        tableForm.setCapacity("5");
        tableForm.setFloorId(floorEntity1.getFlrId().toString());

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableName", "patched table name"),
                new PatchOperationForm("replace", "/description", "patched description"),
                new PatchOperationForm("replace", "/capacity", "1"));

        tableEntity1 = new TableEntity();
        tableEntity1.setTableName("Table 1 Name");
        tableEntity1.setDescription("Table 1 Description");
        tableEntity1.setCapacity("1");
        tableEntity1.setActive(Boolean.TRUE);
        tableEntity1.setFloor(floorEntity1);

        tableEntity1 = tableRepository.save(tableEntity1);

        tableVo1 = new TableVo();
        tableVo1.setTableId(tableEntity1.getTableId().toString());
        tableVo1.setTableName(tableEntity1.getTableName());
        tableVo1.setDescription(tableEntity1.getDescription());
        tableVo1.setCapacity(tableEntity1.getCapacity());
        tableVo1.setFloorId(tableEntity1.getFloor().getFlrId().toString());

        tableEntity2 = new TableEntity();
        tableEntity2.setTableName("Table 2 Name");
        tableEntity2.setDescription("Table 2 Description");
        tableEntity2.setCapacity("2");
        tableEntity2.setActive(Boolean.TRUE);
        tableEntity2.setFloor(floorEntity2);

        tableEntity2 = tableRepository.save(tableEntity2);

        tableVo2 = new TableVo();
        tableVo2.setTableId(tableEntity2.getTableId().toString());
        tableVo2.setTableName(tableEntity2.getTableName());
        tableVo2.setDescription(tableEntity2.getDescription());
        tableVo2.setCapacity(tableEntity2.getCapacity());
        tableVo2.setFloorId(tableEntity2.getFloor().getFlrId().toString());

        tableEntity3 = new TableEntity();
        tableEntity3.setTableName("Table 3 Name");
        tableEntity3.setDescription("Table 3 Description");
        tableEntity3.setCapacity("3");
        tableEntity3.setActive(Boolean.FALSE);
        tableEntity3.setFloor(floorEntity3);

        tableEntity3 = tableRepository.save(tableEntity3);

        tableVo3 = new TableVo();
        tableVo3.setTableId(tableEntity3.getTableId().toString());
        tableVo3.setTableName(tableEntity3.getTableName());
        tableVo3.setDescription(tableEntity3.getDescription());
        tableVo3.setCapacity(tableEntity3.getCapacity());
        tableVo3.setFloorId(tableEntity3.getFloor().getFlrId().toString());

        tableEntity4 = new TableEntity();
        tableEntity4.setTableName("Table 4 Name");
        tableEntity4.setDescription("Table 4 Description");
        tableEntity4.setCapacity("4");
        tableEntity4.setActive(Boolean.TRUE);
        tableEntity4.setFloor(floorEntity3);

        tableEntity4 = tableRepository.save(tableEntity4);

        tableVo4 = new TableVo();
        tableVo4.setTableId(tableEntity4.getTableId().toString());
        tableVo4.setTableName(tableEntity4.getTableName());
        tableVo4.setDescription(tableEntity4.getDescription());
        tableVo4.setCapacity(tableEntity4.getCapacity());
        tableVo4.setFloorId(tableEntity4.getFloor().getFlrId().toString());

        floorEntity1.setTable(new ArrayList<TableEntity>(List.of(tableEntity1)));
        floorEntity1 = floorRepository.save(floorEntity1);
        floorEntity2.setTable(new ArrayList<TableEntity>(List.of(tableEntity2)));
        floorEntity2 = floorRepository.save(floorEntity2);
        floorEntity3.setTable(new ArrayList<TableEntity>(List.of(tableEntity3)));
        floorEntity3 = floorRepository.save(floorEntity3);
        floorEntity4.setTable(new ArrayList<TableEntity>(List.of(tableEntity4)));
        floorEntity4 = floorRepository.save(floorEntity4);

        tableVo1.setFloorVo(floorVo1);
        tableVo2.setFloorVo(floorVo2);
        tableVo3.setFloorVo(floorVo3);
        tableVo4.setFloorVo(floorVo4);

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

        tableRepository.deleteById(tableEntity1.getTableId());
        tableRepository.deleteById(tableEntity2.getTableId());
        tableRepository.deleteById(tableEntity3.getTableId());
        tableRepository.deleteById(tableEntity4.getTableId());

        floorEntity1.setTable(null);
        floorEntity2.setTable(null);
        floorEntity3.setTable(null);
        floorEntity4.setTable(null);

        floorEntity1.setTable(null);
        floorEntity2.setTable(null);
        floorEntity3.setTable(null);
        floorEntity4.setTable(null);

        tableEntity1.setFloor(null);
        tableEntity2.setFloor(null);
        tableEntity3.setFloor(null);
        tableEntity4.setFloor(null);
    }

    @Test
    public void test_Table_Post_ShouldReturn_201Response_And_NewTableId_WhenPosted_WithValidTableForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(TABLE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Table_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyTableName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableName";
        tableForm.setTableName("");

        mvcResult = mockMvc.perform(post(TABLE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Table_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        tableForm.setDescription("");

        mvcResult = mockMvc.perform(post(TABLE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Table_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyCapacity() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "capacity";
        tableForm.setCapacity("");

        mvcResult = mockMvc.perform(post(TABLE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Table_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithInvalidCapacity() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "capacity";
        tableForm.setCapacity("r");

        mvcResult = mockMvc.perform(post(TABLE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Table_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        tableForm.setFloorId("");

        mvcResult = mockMvc.perform(post(TABLE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Table_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithInvalidFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        tableForm.setFloorId("r");

        mvcResult = mockMvc.perform(post(TABLE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Table_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithAbsentFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        tableForm.setFloorId("99999");

        mvcResult = mockMvc.perform(post(TABLE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Table_Post_ShouldReturn_409Response_And_ErrorCode_RES_CUST_004_WhenRequested_WithDuplicateTable() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String field1Name = "tableName";
        String field2Name = "description";
        String field3Name = "capacity";
        String field4Name = "floorId";
        String field1Value = tableEntity1.getTableName();
        String field2Value = tableEntity1.getDescription();
        String field3Value = tableEntity1.getCapacity();
        String field4Value = tableEntity1.getFloor().getFlrId().toString();
        tableForm.setTableName(field1Value);
        tableForm.setDescription(field2Value);
        tableForm.setCapacity(field3Value);
        tableForm.setFloorId(field4Value);

        mvcResult = mockMvc.perform(post(TABLE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field4Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field4Value));
    }

    @Test
    public void test_Table_Post_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenPosted_WithNoTableForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(TABLE_URI)
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
    public void test_Table_Get_ShouldReturn_200Response_And_TableListNaturallyOrdered_WhenRequested_ForAllTables() throws Exception {
        MvcResult mvcResult = null;
        List<TableVo> tableList = new ArrayList<>(Arrays.asList(tableVo1, tableVo2, tableVo3, tableVo4));

        mvcResult = this.mockMvc.perform(get(TABLE_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo[].class).length);
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_TableListNaturallyOrdered_WhenRequested_ForTables_ByFloorId() throws Exception {
        MvcResult mvcResult = null;

        List<TableVo> tableList = Arrays.asList(tableVo1);
        tableVo1.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_FLOOR_ID, floorEntity1.getFlrId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(tableList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_TableListNaturallyOrdered_WhenRequested_ForTables_ByEmptyFloorId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_FLOOR_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Get_ShouldReturn_404Response_And_ErrorCode_RES_CUST_001_WhenRequested_ByAbsentFloorId() throws Exception {
        MvcResult mvcResult = null;
        String floorId = "kk";
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_FLOOR_ID, floorId))
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
    public void test_Table_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyTableLine1Only(String tableName) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(TABLE_URI_FILTER).queryParam("tableName", tableName))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_EmptyTableList_WhenRequestedBy_AbsentTableName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TABLE_URI_FILTER).queryParam("tableName", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), TableVo[].class).length);
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_TableListNaturallyOrdered_WhenRequested_ForTables_WithTableName() throws Exception {
        MvcResult mvcResult = null;
        List<TableVo> tableList = new ArrayList<>(Arrays.asList(tableVo1));
        tableVo1.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(TABLE_URI_FILTER)
                        .queryParam("tableName", "Table 1 Name"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(tableList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_TableListNaturallyOrdered_WhenRequested_ForTables_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<TableVo> tableList = new ArrayList<>(Arrays.asList(tableVo1));
        tableVo1.setFloorVo(null);
        mvcResult = this.mockMvc.perform(get(TABLE_URI_FILTER)
                        .queryParam("description", "Table 1 Description"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(tableList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_TableListNaturallyOrdered_WhenRequested_ForTables_WithTableNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        List<TableVo> tableList = Arrays.asList(tableVo2);
        tableVo2.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(TABLE_URI_FILTER)
                        .queryParam("tableName", "Table 2 Name")
                        .queryParam("description", "Table 2 Description"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(tableList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_TableDetails_WhenRequested_ById() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        tableVo1.setFloorVo(null);

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(tableVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(tableVo1.getTableId(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getTableId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Table_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = tableEntity2.getTableId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableVo1.getTableId(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getTableId());
        Assertions.assertEquals(tableVo1.getTableName(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getTableName());
        Assertions.assertEquals(tableVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getDescription());
        Assertions.assertEquals(tableVo1.getCapacity(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getCapacity());
        Assertions.assertEquals(tableVo1.getFloorId(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getFloorId());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getActive()));
    }

    @Test
    public void test_Table_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TABLE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(tableVo1.getTableId(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getTableId());
        Assertions.assertEquals(tableVo1.getTableName(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getTableName());
        Assertions.assertEquals(tableVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getDescription());
        Assertions.assertEquals(tableVo1.getCapacity(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getCapacity());
        Assertions.assertEquals(tableVo1.getFloorVo().getFlrId(), om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getFloorVo().getFlrId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TableVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Table_Delete_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001__WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Delete_ShouldReturn_400Response_And_ErrorCode_RES_CUST_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = tableEntity3.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Table_Delete_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTableDetails() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        tableForm.setTableName("Ferran");
        tableForm.setDescription("Description");
        tableForm.setCapacity("6");
        tableForm.setFloorId(floorEntity1.getFlrId().toString());

        mvcResult = this.mockMvc.perform(put(TABLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Table_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenUpdatedBy_EmptyInvalidId_AndTableDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TABLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Put_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByAbsentId_AndTableDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TABLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Table_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_005_WhenUpdated_ByInactiveId_AndTableDetails() throws Exception {
        String id = tableEntity3.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(TABLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Table_Put_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndNoTableDetails() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(TABLE_URI_BY_ID, id)
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
    public void test_Table_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndEmptyTableName(String tableName) throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableName";
        tableForm.setTableName(tableName);

        mvcResult = mockMvc.perform(put(TABLE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Table_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndEmptyInvalidFloorId(String floorId) throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        tableForm.setFloorId(floorId);

        mvcResult = mockMvc.perform(put(TABLE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999" })
    public void test_Table_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndAbsentFloorId(String floorId) throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        tableForm.setFloorId(floorId);

        mvcResult = mockMvc.perform(put(TABLE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Table_Put_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndEmptyTableDetails() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(TABLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new TableForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Table_Put_ShouldReturn_409Response_And_ErrorCode_RES_CUST_004_WhenUpdated_ById_AndDuplicateTableDetails() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String field1Name = "tableName";
        String field2Name = "description";
        String field3Name = "capacity";
        String field4Name = "floorId";
        String field1Value = tableEntity2.getTableName();
        String field2Value = tableEntity2.getDescription();
        String field3Value = tableEntity2.getCapacity();
        String field4Value = tableEntity2.getFloor().getFlrId().toString();
        tableForm.setTableName(field1Value);
        tableForm.setDescription(field2Value);
        tableForm.setCapacity(field3Value);
        tableForm.setFloorId(field4Value);

        mvcResult = mockMvc.perform(put(TABLE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(tableForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field4Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field4Value));
    }

    @Test
    public void test_Table_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTableDetails() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(TABLE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndTableDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TABLE_URI_BY_ID, " ")
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
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByInvalidId_AndTableDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByAbsentId_AndTableDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_409Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ById_AndDuplicateTableDetails() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getErrorCode();
        String field1Name = "tableName";
        String field2Name = "floorId";
        String field3Name = "description";
        String field4Name = "capacity";
        String field1Value = tableEntity2.getTableName();
        String field2Value = tableEntity2.getFloor().getFlrId().toString();
        String field3Value = tableEntity2.getDescription();
        String field4Value = tableEntity2.getCapacity();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value),
                new PatchOperationForm("replace", "/" + field3Name, field3Value),
                new PatchOperationForm("replace", "/" + field4Name, field4Value));


        mvcResult = this.mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndNoTableDetails() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(TABLE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableName", " "));

        mvcResult = mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyFloorId(String floorId) throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/floorId", floorId));

        mvcResult = mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCapacity(String capacity) throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/capacity", capacity));

        mvcResult = mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidCapacity(String capacity) throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "capacity";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, capacity));

        mvcResult = mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidFloorId(String floorId) throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "floorId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, floorId));

        mvcResult = mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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
    public void test_Table_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidDefinitionOfTableAttribute() throws Exception {
        String id = tableEntity1.getTableId().toString();
        MvcResult mvcResult = null;
        String errorCode = EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(TABLE_URI_BY_ID, id)
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

