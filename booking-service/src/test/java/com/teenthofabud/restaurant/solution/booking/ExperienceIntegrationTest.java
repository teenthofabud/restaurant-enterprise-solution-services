package com.teenthofabud.restaurant.solution.booking;


import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceDocument;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceForm;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceVo;
import com.teenthofabud.restaurant.solution.booking.experience.repository.ExperienceRepository;
import com.teenthofabud.restaurant.solution.booking.error.BookingErrorCode;
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
public class ExperienceIntegrationTest extends BookingIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String CHARGE_URI = "/experience";
    private static final String CHARGE_URI_BY_ID = "/experience/{id}";
    private static final String CHARGE_URI_FILTER = "/experience/filter";

    private ExperienceRepository experienceRepository;

    @Autowired
    public void setExperienceRepository(ExperienceRepository experienceRepository) {
        this.experienceRepository = experienceRepository;
    }
    
    private ExperienceForm experienceForm;
    private ExperienceVo experienceVo1;
    private ExperienceVo experienceVo2;
    private ExperienceVo experienceVo3;
    private ExperienceVo experienceVo4;
    private ExperienceVo experienceVo5;
    private ExperienceDocument experienceDocument1;
    private ExperienceDocument experienceDocument2;
    private ExperienceDocument experienceDocument3;
    private ExperienceDocument experienceDocument4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Experience
         */

        experienceForm = new ExperienceForm();
        experienceForm.setName("New Name");
        experienceForm.setDescription("New Description");
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/description", "patched description"));

        experienceDocument1 = new ExperienceDocument();
        experienceDocument1.setName("Experience 1 Name");
        experienceDocument1.setDescription("Experience 1 Description");
        experienceDocument1.setActive(Boolean.TRUE);

        experienceDocument2 = new ExperienceDocument();
        experienceDocument2.setName("Experience 2 Name");
        experienceDocument2.setDescription("Experience 2 Description");
        experienceDocument2.setActive(Boolean.TRUE);

        experienceDocument3 = new ExperienceDocument();
        experienceDocument3.setName("Experience 3 Name");
        experienceDocument3.setDescription("Experience 3 Description");
        experienceDocument3.setActive(Boolean.TRUE);

        experienceDocument4 = new ExperienceDocument();
        experienceDocument4.setName("Experience 4 Name");
        experienceDocument4.setDescription("Experience 4 Description");
        experienceDocument4.setActive(Boolean.FALSE);

        experienceDocument1 = experienceRepository.save(experienceDocument1);

        experienceVo1 = new ExperienceVo();
        experienceVo1.setId(experienceDocument1.getId().toString());
        experienceVo1.setName(experienceDocument1.getName());
        experienceVo1.setDescription(experienceDocument1.getDescription());

        experienceDocument2 = experienceRepository.save(experienceDocument2);

        experienceVo2 = new ExperienceVo();
        experienceVo2.setId(experienceDocument2.getId().toString());
        experienceVo2.setName(experienceDocument2.getName());
        experienceVo2.setDescription(experienceDocument2.getDescription());

        experienceDocument3 = experienceRepository.save(experienceDocument3);

        experienceVo3 = new ExperienceVo();
        experienceVo3.setId(experienceDocument3.getId().toString());
        experienceVo3.setName(experienceDocument3.getName());
        experienceVo3.setDescription(experienceDocument3.getDescription());

        experienceDocument4 = experienceRepository.save(experienceDocument4);

        experienceVo4 = new ExperienceVo();
        experienceVo4.setId(experienceDocument4.getId().toString());
        experienceVo4.setName(experienceDocument4.getName());
        experienceVo4.setDescription(experienceDocument4.getDescription());

        experienceVo5 = new ExperienceVo();
        experienceVo5.setId(UUID.randomUUID().toString());
        experienceVo5.setName(experienceForm.getName());
        experienceVo5.setDescription(experienceForm.getDescription());

        experienceDocument1 = experienceRepository.save(experienceDocument1);
        experienceDocument2 = experienceRepository.save(experienceDocument2);
        experienceDocument3 = experienceRepository.save(experienceDocument3);
        experienceDocument4 = experienceRepository.save(experienceDocument4);

    }

    @AfterEach
    private void destroy() {
        experienceRepository.deleteById(experienceDocument1.getId());
        experienceRepository.deleteById(experienceDocument2.getId());
        experienceRepository.deleteById(experienceDocument3.getId());
        experienceRepository.deleteById(experienceDocument4.getId());
    }

    @Test
    public void test_Experience_Post_ShouldReturn_201Response_And_NewExperienceId_WhenPosted_WithValidExperienceForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Experience_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        experienceForm.setName("");

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Experience_Post_ShouldReturn_201Response_And_NewExperienceId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        experienceForm.setName("New Experience Name");
        experienceForm.setDescription("");

        mvcResult = mockMvc.perform(post(CHARGE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Experience_Post_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenPosted_WithNoExperienceForm() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(CHARGE_URI)
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
    public void test_Experience_Get_ShouldReturn_200Response_And_ExperienceListNaturallyOrdered_WhenRequested_ForAllExperiences() throws Exception {
        MvcResult mvcResult = null;
        Set<ExperienceVo> experienceList = new TreeSet<>(Arrays.asList(experienceVo1, experienceVo2, experienceVo3, experienceVo4, experienceVo5));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(experienceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Experience_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Experience_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_EmptyExperienceList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo[].class).length);
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_EmptyExperienceList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo[].class).length);
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_ExperienceListNaturallyOrdered_WhenRequested_ForExperiences_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<ExperienceVo> experienceList = new ArrayList<>(Arrays.asList(experienceVo1, experienceVo2, experienceVo3, experienceVo4, experienceVo5));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("name", "Experience"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(experienceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo[].class).length);
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_ExperienceListNaturallyOrdered_WhenRequested_ForExperiences_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<ExperienceVo> experienceList = new ArrayList<>(Arrays.asList(experienceVo2));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("description", "Experience 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(experienceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo[].class).length);
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_ExperienceListNaturallyOrdered_WhenRequested_ForExperiences_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<ExperienceVo> experienceList = new TreeSet<>(Arrays.asList(experienceVo1));

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("name", "Experience 1")
                        .queryParam("description", "Experience 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(experienceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo[].class).length);
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_EmptyExperienceList_WhenRequested_ForExperiences_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<ExperienceVo> experienceList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_FILTER)
                        .queryParam("name", "Experience 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(experienceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo[].class).length);
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_ExperienceDetails_WhenRequested_ById() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(experienceVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(experienceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Experience_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = experienceDocument3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(experienceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getId());
        Assertions.assertEquals(experienceVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getName());
        Assertions.assertEquals(experienceVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getActive()));
    }

    @Test
    public void test_Experience_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(CHARGE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(experienceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getId());
        Assertions.assertEquals(experienceVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getName());
        Assertions.assertEquals(experienceVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), ExperienceVo.class).getActive()));
    }

    @Test
    public void test_Experience_Delete_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Delete_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Delete_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = experienceDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Experience_Delete_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndExperienceDetails() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        experienceForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Experience_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenUpdatedBy_EmptyId_AndExperienceDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Put_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ByAbsentId_AndExperienceDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Experience_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_005_WhenUpdated_ByInactiveId_AndExperienceDetails() throws Exception {
        String id = experienceDocument4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Experience_Put_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndNoExperienceDetails() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
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
    public void test_Experience_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        experienceForm.setName("");

        mvcResult = mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        experienceForm.setDescription("");

        mvcResult = mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(experienceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Experience_Put_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndEmptyExperienceDetails() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(CHARGE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(new ExperienceForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Experience_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndExperienceDetails() throws Exception {
        String id = experienceDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Experience_Patch_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenUpdated_ByEmptyId_AndExperienceDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, " ")
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
    public void test_Experience_Patch_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ByAbsentId_AndExperienceDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Experience_Patch_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndNoExperienceDetails() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(CHARGE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Experience_Patch_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Experience_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Experience_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyRate() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/rate", " "));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public void test_Experience_Patch_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidDefinitionOfExperienceAttribute() throws Exception {
        String id = experienceDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(CHARGE_URI_BY_ID, id)
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
    public String getSimulationBaseLocation() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("simulation base location not available");
    }

    @Override
    public Integer getServicePort() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("service port not available");
    }

    @Override
    public String[] getSimulationFilePaths() throws UnsupportedOperationException {
        throw new UnsupportedOperationException("simulation file path not available");
    }
}
