package com.teenthofabud.restaurant.solution.settings;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceDocument;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceForm;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceType;
import com.teenthofabud.restaurant.solution.settings.device.data.DeviceVo;
import com.teenthofabud.restaurant.solution.settings.device.repository.DeviceRepository;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
public class DeviceIntegrationTest extends SettingsIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String DEVICE_URI = "/device";
    private static final String DEVICE_URI_BY_ID = "/device/{id}";
    private static final String DEVICE_URI_FILTER = "/device/filter";

    private DeviceRepository deviceRepository;

    @Autowired
    public void setDeviceRepository(DeviceRepository deviceRepository) {
        this.deviceRepository = deviceRepository;
    }

    private DeviceForm deviceForm;
    private DeviceVo deviceVo1;
    private DeviceVo deviceVo2;
    private DeviceVo deviceVo3;
    private DeviceVo deviceVo4;
    private DeviceVo deviceVo5;
    private DeviceVo deviceVo6;
    private DeviceDocument deviceDocument1;
    private DeviceDocument deviceDocument2;
    private DeviceDocument deviceDocument3;
    private DeviceDocument deviceDocument4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Device
         */

        deviceForm = new DeviceForm();
        deviceForm.setName("New Name");
        deviceForm.setDescription("New Description");
        deviceForm.setDeviceTypeId(DeviceType.PRINT.name());
        deviceForm.setLocation("New Device Location");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/location", "patched location"),
                new PatchOperationForm("replace", "/description", "patched description"));

        deviceDocument1 = new DeviceDocument();
        deviceDocument1.setName("Device 1 Name");
        deviceDocument1.setDescription("Device 1 Description");
        deviceDocument1.setDeviceTypeId(DeviceType.PRINT.name());
        deviceDocument1.setLocation("Device 1 Location");
        deviceDocument1.setActive(Boolean.TRUE);

        deviceDocument2 = new DeviceDocument();
        deviceDocument2.setName("Device 2 Name");
        deviceDocument2.setDescription("Device 2 Description");
        deviceDocument2.setDeviceTypeId(DeviceType.PRINT.name());
        deviceDocument2.setLocation("Device 2 Location");
        deviceDocument2.setActive(Boolean.TRUE);

        deviceDocument3 = new DeviceDocument();
        deviceDocument3.setName("Device 3 Name");
        deviceDocument3.setDescription("Device 3 Description");
        deviceDocument3.setDeviceTypeId(DeviceType.PRINT.name());
        deviceDocument3.setLocation("Device 3 Location");
        deviceDocument3.setActive(Boolean.TRUE);

        deviceDocument4 = new DeviceDocument();
        deviceDocument4.setName("Device 4 Name");
        deviceDocument4.setDescription("Device 4 Description");
        deviceDocument4.setDeviceTypeId(DeviceType.PRINT.name());
        deviceDocument4.setLocation("Device 4 Location");
        deviceDocument4.setActive(Boolean.FALSE);

        deviceDocument1 = deviceRepository.save(deviceDocument1);

        deviceVo1 = new DeviceVo();
        deviceVo1.setId(deviceDocument1.getId().toString());
        deviceVo1.setName(deviceDocument1.getName());
        deviceVo1.setDescription(deviceDocument1.getDescription());
        deviceVo1.setLocation(deviceDocument1.getLocation());
        deviceVo1.setDeviceTypeId(deviceDocument1.getDeviceTypeId());

        deviceDocument2 = deviceRepository.save(deviceDocument2);

        deviceVo2 = new DeviceVo();
        deviceVo2.setId(deviceDocument2.getId().toString());
        deviceVo2.setName(deviceDocument2.getName());
        deviceVo2.setDescription(deviceDocument2.getDescription());
        deviceVo2.setLocation(deviceDocument2.getLocation());
        deviceVo2.setDeviceTypeId(deviceDocument2.getDeviceTypeId());

        deviceDocument3 = deviceRepository.save(deviceDocument3);

        deviceVo3 = new DeviceVo();
        deviceVo3.setId(deviceDocument3.getId().toString());
        deviceVo3.setName(deviceDocument3.getName());
        deviceVo3.setDescription(deviceDocument3.getDescription());
        deviceVo3.setLocation(deviceDocument3.getLocation());
        deviceVo3.setDeviceTypeId(deviceDocument3.getDeviceTypeId());

        deviceDocument4 = deviceRepository.save(deviceDocument4);

        deviceVo4 = new DeviceVo();
        deviceVo4.setId(deviceDocument4.getId().toString());
        deviceVo4.setName(deviceDocument4.getName());
        deviceVo4.setDescription(deviceDocument4.getDescription());
        deviceVo4.setLocation(deviceDocument4.getLocation());
        deviceVo4.setDeviceTypeId(deviceDocument4.getDeviceTypeId());

        deviceVo5 = new DeviceVo();
        deviceVo5.setId(UUID.randomUUID().toString());
        deviceVo5.setName(deviceForm.getName());
        deviceVo5.setDescription(deviceForm.getDescription());
        deviceVo5.setLocation(deviceForm.getLocation());
        deviceVo5.setDeviceTypeId(deviceForm.getDeviceTypeId());

        deviceVo6 = new DeviceVo();
        deviceVo6.setId(UUID.randomUUID().toString());
        deviceVo6.setName("Another Name");
        deviceVo6.setDescription("");
        deviceVo6.setLocation(deviceForm.getLocation());
        deviceVo6.setDeviceTypeId(deviceForm.getDeviceTypeId());

    }

    @AfterEach
    private void destroy() {
        deviceRepository.deleteById(deviceDocument1.getId());
        deviceRepository.deleteById(deviceDocument2.getId());
        deviceRepository.deleteById(deviceDocument3.getId());
        deviceRepository.deleteById(deviceDocument4.getId());

        deviceForm = new DeviceForm();
        deviceForm.setName("New Name");
        deviceForm.setDescription("New Description");
        deviceForm.setDeviceTypeId(DeviceType.PRINT.name());
        deviceForm.setLocation("New Device Location");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/location", "patched location"),
                new PatchOperationForm("replace", "/description", "patched description"));

    }

    @Test
    public void test_Device_Post_ShouldReturn_201Response_And_NewDeviceId_WhenPosted_WithValidDeviceForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(DEVICE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Device_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        deviceForm.setName("");

        mvcResult = mockMvc.perform(post(DEVICE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Device_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithEmptyDeviceTypeId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "deviceTypeId";
        deviceForm.setDeviceTypeId("");

        mvcResult = mockMvc.perform(post(DEVICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Device_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithInvalidDeviceTypeId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "deviceTypeId";
        deviceForm.setDeviceTypeId("r");

        mvcResult = mockMvc.perform(post(DEVICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Device_Post_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_WithEmptyLocation() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "location";
        deviceForm.setLocation("");

        mvcResult = mockMvc.perform(post(DEVICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Device_Post_ShouldReturn_201Response_And_NewDeviceId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        deviceForm.setName("Another Name");
        deviceForm.setDescription("");

        mvcResult = mockMvc.perform(post(DEVICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Device_Post_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenPosted_WithNoDeviceForm() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(DEVICE_URI)
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
    public void test_Device_Get_ShouldReturn_200Response_And_DeviceListNaturallyOrdered_WhenRequested_ForAllDevices() throws Exception {
        MvcResult mvcResult = null;
        Set<DeviceVo> deviceList = new TreeSet<>(Arrays.asList(deviceVo1, deviceVo2, deviceVo3, deviceVo4, deviceVo5));

        mvcResult = this.mockMvc.perform(get(DEVICE_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Device_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Device_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Device_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyDeviceTypeIdOnly(String deviceTypeId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER).queryParam("deviceTypeId", deviceTypeId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_EmptyDeviceList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_EmptyDeviceList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DeviceListNaturallyOrdered_WhenRequested_ForDevices_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<DeviceVo> deviceList = new ArrayList<>(Arrays.asList(deviceVo1, deviceVo2, deviceVo3, deviceVo4));

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER)
                        .queryParam("name", "Device"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DeviceListNaturallyOrdered_WhenRequested_ForDevices_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<DeviceVo> deviceList = new ArrayList<>(Arrays.asList(deviceVo2));

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER)
                        .queryParam("description", "Device 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DeviceListNaturallyOrdered_WhenRequested_ForDevices_WithDeviceTypeId() throws Exception {
        MvcResult mvcResult = null;
        List<DeviceVo> deviceList = new ArrayList<>(Arrays.asList(deviceVo1, deviceVo2, deviceVo3, deviceVo4));

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER)
                        .queryParam("deviceTypeId", "PRINT"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DeviceListNaturallyOrdered_WhenRequested_ForDevices_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<DeviceVo> deviceList = new TreeSet<>(Arrays.asList(deviceVo1));

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER)
                        .queryParam("name", "Device 1")
                        .queryParam("description", "Device 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DeviceListNaturallyOrdered_WhenRequested_ForDevices_WithNameAndDescriptionAndDeviceTypeId() throws Exception {
        MvcResult mvcResult = null;
        Set<DeviceVo> deviceList = new TreeSet<>(Arrays.asList(deviceVo1));

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER)
                        .queryParam("name", "Device 1")
                        .queryParam("deviceTypeId", "PRINT")
                        .queryParam("description", "Device 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_EmptyDeviceList_WhenRequested_ForDevices_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<DeviceVo> deviceList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER)
                        .queryParam("name", "Device 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_EmptyDeviceList_WhenRequested_ForDevices_WithAbsent_WithNameAndDescriptionAndDeviceTypeId() throws Exception {
        MvcResult mvcResult = null;
        Set<DeviceVo> deviceList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_FILTER)
                        .queryParam("name", "Device 1")
                        .queryParam("deviceTypeId", "PRINT")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo[].class).length);
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DeviceDetails_WhenRequested_ById() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(deviceVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(deviceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Device_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Device_Get_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenRequestedBy_InvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Get_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = deviceDocument3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getId());
        Assertions.assertEquals(deviceVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getName());
        Assertions.assertEquals(deviceVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getDescription());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getActive()));
    }

    @Test
    public void test_Device_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(DEVICE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(deviceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getId());
        Assertions.assertEquals(deviceVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getName());
        Assertions.assertEquals(deviceVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), DeviceVo.class).getActive()));
    }

    @Test
    public void test_Device_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DEVICE_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Delete_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Delete_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = deviceDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Device_Delete_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDeviceDetails() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        deviceForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Device_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenUpdatedBy_EmptyInvalidId_AndDeviceDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Device_Put_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdatedBy_EmptyAbsentId_AndDeviceDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Put_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndDeviceDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Device_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_005_WhenUpdated_ByInactiveId_AndDeviceDetails() throws Exception {
        String id = deviceDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Device_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoDeviceDetails() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(DEVICE_URI_BY_ID, id)
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
    public void test_Device_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        deviceForm.setName("");

        mvcResult = mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        deviceForm.setDescription("");

        mvcResult = mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Device_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDeviceTypeId(String deviceTypeId) throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "deviceTypeId";
        deviceForm.setDeviceTypeId(deviceTypeId);

        mvcResult = mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Device_Put_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidLocation(String location) throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "location";
        deviceForm.setLocation(location);

        mvcResult = mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(deviceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Device_Put_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndEmptyDeviceDetails() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(DEVICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new DeviceForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Device_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndDeviceDetails() throws Exception {
        String id = deviceDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(DEVICE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Device_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndDeviceDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(DEVICE_URI_BY_ID, " ")
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
    public void test_Device_Patch_ShouldReturn_404Response_And_ErrorCode_RES_SETTINGS_002_WhenUpdated_ByAbsentId_AndDeviceDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(DEVICE_URI_BY_ID, id)
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
    public void test_Device_Patch_ShouldReturn_422Response_And_ErrorCode_RES_SETTINGS_003_WhenUpdated_ById_AndNoDeviceDetails() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(DEVICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Device_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(DEVICE_URI_BY_ID, id)
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
    public void test_Device_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(DEVICE_URI_BY_ID, id)
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
    public void test_Device_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDeviceTypeId() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "deviceTypeId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "r"));

        mvcResult = mockMvc.perform(patch(DEVICE_URI_BY_ID, id)
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
    public void test_Device_Patch_ShouldReturn_400Response_And_ErrorCode_RES_SETTINGS_001_WhenRequested_ById_AndInvalidDefinitionOfDeviceAttribute() throws Exception {
        String id = deviceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = SettingsErrorCode.SETTINGS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(DEVICE_URI_BY_ID, id)
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
