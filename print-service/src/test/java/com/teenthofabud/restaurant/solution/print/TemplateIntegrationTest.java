package com.teenthofabud.restaurant.solution.print;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateEntity;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateForm;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateType;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateVo;
import com.teenthofabud.restaurant.solution.print.template.repository.TemplateRepository;
import com.teenthofabud.restaurant.solution.print.error.PrintErrorCode;
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
public class TemplateIntegrationTest extends PrintIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String TEMPLATE_URI = "/template";
    private static final String TEMPLATE_URI_BY_ID = "/template/{id}";
    private static final String TEMPLATE_URI_FILTER = "/template/filter";

    private TemplateRepository templateRepository;

    @Autowired
    public void setTemplateRepository(TemplateRepository templateRepository) {
        this.templateRepository = templateRepository;
    }

    private TemplateForm templateForm;
    private TemplateVo templateVo1;
    private TemplateVo templateVo2;
    private TemplateVo templateVo3;
    private TemplateVo templateVo4;
    private TemplateVo templateVo5;
    private TemplateEntity templateEntity1;
    private TemplateEntity templateEntity2;
    private TemplateEntity templateEntity3;
    private TemplateEntity templateEntity4;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Template
         */

        templateForm = new TemplateForm();
        templateForm.setName("New Name");
        templateForm.setDescription("New Description");
        templateForm.setTemplateTypeId(TemplateType.FREEMARKER.name());
        templateForm.setContent("New Template Content ${content}");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "patched name"),
                new PatchOperationForm("replace", "/content", "patched content ${content}"),
                new PatchOperationForm("replace", "/description", "patched description"));

        templateEntity1 = new TemplateEntity();
        templateEntity1.setName("Template 1 Name");
        templateEntity1.setDescription("Template 1 Description");
        templateEntity1.setTemplateTypeId(TemplateType.FREEMARKER.name());
        templateEntity1.setContent("Template 1 Content ${content}");
        templateEntity1.setActive(Boolean.TRUE);

        templateEntity2 = new TemplateEntity();
        templateEntity2.setName("Template 2 Name");
        templateEntity2.setDescription("Template 2 Description");
        templateEntity2.setTemplateTypeId(TemplateType.FREEMARKER.name());
        templateEntity2.setContent("Template 2 Content ${content}");
        templateEntity2.setActive(Boolean.TRUE);

        templateEntity3 = new TemplateEntity();
        templateEntity3.setName("Template 3 Name");
        templateEntity3.setDescription("Template 3 Description");
        templateEntity3.setTemplateTypeId(TemplateType.FREEMARKER.name());
        templateEntity3.setContent("Template 3 Content ${content}");
        templateEntity3.setActive(Boolean.TRUE);

        templateEntity4 = new TemplateEntity();
        templateEntity4.setName("Template 4 Name");
        templateEntity4.setDescription("Template 4 Description");
        templateEntity4.setTemplateTypeId(TemplateType.FREEMARKER.name());
        templateEntity4.setContent("Template 4 Content ${content}");
        templateEntity4.setActive(Boolean.FALSE);

        templateEntity1 = templateRepository.save(templateEntity1);

        templateVo1 = new TemplateVo();
        templateVo1.setId(templateEntity1.getId().toString());
        templateVo1.setName(templateEntity1.getName());
        templateVo1.setDescription(templateEntity1.getDescription());
        templateVo1.setContent(templateEntity1.getContent());
        templateVo1.setTemplateTypeId(templateEntity1.getTemplateTypeId());

        templateEntity2 = templateRepository.save(templateEntity2);

        templateVo2 = new TemplateVo();
        templateVo2.setId(templateEntity2.getId().toString());
        templateVo2.setName(templateEntity2.getName());
        templateVo2.setDescription(templateEntity2.getDescription());
        templateVo2.setContent(templateEntity2.getContent());
        templateVo2.setTemplateTypeId(templateEntity2.getTemplateTypeId());

        templateEntity3 = templateRepository.save(templateEntity3);

        templateVo3 = new TemplateVo();
        templateVo3.setId(templateEntity3.getId().toString());
        templateVo3.setName(templateEntity3.getName());
        templateVo3.setDescription(templateEntity3.getDescription());
        templateVo3.setContent(templateEntity3.getContent());
        templateVo3.setTemplateTypeId(templateEntity3.getTemplateTypeId());

        templateEntity4 = templateRepository.save(templateEntity4);

        templateVo4 = new TemplateVo();
        templateVo4.setId(templateEntity4.getId().toString());
        templateVo4.setName(templateEntity4.getName());
        templateVo4.setDescription(templateEntity4.getDescription());
        templateVo4.setContent(templateEntity4.getContent());
        templateVo4.setTemplateTypeId(templateEntity4.getTemplateTypeId());

        templateVo5 = new TemplateVo();
        templateVo5.setId(UUID.randomUUID().toString());
        templateVo5.setName(templateForm.getName());
        templateVo5.setDescription(templateForm.getDescription());
        templateVo5.setContent(templateForm.getContent());
        templateVo5.setTemplateTypeId(templateForm.getTemplateTypeId());

    }

    @AfterEach
    private void destroy() {
        templateRepository.deleteById(templateEntity1.getId());
        templateRepository.deleteById(templateEntity2.getId());
        templateRepository.deleteById(templateEntity3.getId());
        templateRepository.deleteById(templateEntity4.getId());
    }

    @Test
    public void test_Template_Post_ShouldReturn_201Response_And_NewTemplateId_WhenPosted_WithValidTemplateForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(TEMPLATE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Template_Post_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        templateForm.setName("");

        mvcResult = mockMvc.perform(post(TEMPLATE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Template_Post_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_WithEmptyTemplateTypeId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "templateTypeId";
        templateForm.setTemplateTypeId("");

        mvcResult = mockMvc.perform(post(TEMPLATE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Template_Post_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_WithInvalidTemplateTypeId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "templateTypeId";
        templateForm.setTemplateTypeId("r");

        mvcResult = mockMvc.perform(post(TEMPLATE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Template_Post_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_WithEmptyContent() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "content";
        templateForm.setContent("");

        mvcResult = mockMvc.perform(post(TEMPLATE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Template_Post_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_WithInvalidContent() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "content";
        templateForm.setContent("r ${}");

        mvcResult = mockMvc.perform(post(TEMPLATE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Template_Post_ShouldReturn_201Response_And_NewTemplateId_WhenPosted_WithEmptyDescription() throws Exception {
        MvcResult mvcResult = null;
        templateForm.setDescription("");

        mvcResult = mockMvc.perform(post(TEMPLATE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Template_Post_ShouldReturn_422Response_And_ErrorCode_RES_PRINT_003_WhenPosted_WithNoTemplateForm() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(TEMPLATE_URI)
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
    public void test_Template_Get_ShouldReturn_200Response_And_TemplateListNaturallyOrdered_WhenRequested_ForAllTemplates() throws Exception {
        MvcResult mvcResult = null;
        Set<TemplateVo> templateList = new TreeSet<>(Arrays.asList(templateVo1, templateVo2, templateVo3, templateVo4));

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Template_Get_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequestedBy_EmptyNameOnly(String name) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER).queryParam("name", name))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Template_Get_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequestedBy_EmptyDescriptionOnly(String description) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER).queryParam("description", description))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Template_Get_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequestedBy_EmptyTemplateTypeIdOnly(String templateTypeId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER).queryParam("templateTypeId", templateTypeId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_EmptyTemplateList_WhenRequestedBy_AbsentName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER).queryParam("name", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_EmptyTemplateList_WhenRequestedBy_AbsentDescription() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER).queryParam("description", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_TemplateListNaturallyOrdered_WhenRequested_ForTemplates_WithName() throws Exception {
        MvcResult mvcResult = null;
        List<TemplateVo> templateList = new ArrayList<>(Arrays.asList(templateVo1, templateVo2, templateVo3, templateVo4));

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER)
                        .queryParam("name", "Template"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_TemplateListNaturallyOrdered_WhenRequested_ForTemplates_WithDescription() throws Exception {
        MvcResult mvcResult = null;
        List<TemplateVo> templateList = new ArrayList<>(Arrays.asList(templateVo2));

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER)
                        .queryParam("description", "Template 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_TemplateListNaturallyOrdered_WhenRequested_ForTemplates_WithTemplateTypeId() throws Exception {
        MvcResult mvcResult = null;
        List<TemplateVo> templateList = new ArrayList<>(Arrays.asList(templateVo1, templateVo2, templateVo3, templateVo4));

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER)
                        .queryParam("templateTypeId", "FREEMARKER"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_TemplateListNaturallyOrdered_WhenRequested_ForTemplates_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<TemplateVo> templateList = new TreeSet<>(Arrays.asList(templateVo1));

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER)
                        .queryParam("name", "Template 1")
                        .queryParam("description", "Template 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_TemplateListNaturallyOrdered_WhenRequested_ForTemplates_WithNameAndDescriptionAndTemplateTypeId() throws Exception {
        MvcResult mvcResult = null;
        Set<TemplateVo> templateList = new TreeSet<>(Arrays.asList(templateVo1));

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER)
                        .queryParam("name", "Template 1")
                        .queryParam("templateTypeId", "FREEMARKER")
                        .queryParam("description", "Template 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_EmptyTemplateList_WhenRequested_ForTemplates_WithAbsent_WithNameAndDescription() throws Exception {
        MvcResult mvcResult = null;
        Set<TemplateVo> templateList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER)
                        .queryParam("name", "Template 1")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_EmptyTemplateList_WhenRequested_ForTemplates_WithAbsent_WithNameAndDescriptionAndTemplateTypeId() throws Exception {
        MvcResult mvcResult = null;
        Set<TemplateVo> templateList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_FILTER)
                        .queryParam("name", "Template 1")
                        .queryParam("templateTypeId", "FREEMARKER")
                        .queryParam("description", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo[].class).length);
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_TemplateDetails_WhenRequested_ById() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(templateVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(templateVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Template_Get_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Get_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = templateEntity3.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getId());
        Assertions.assertEquals(templateVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getName());
        Assertions.assertEquals(templateVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getActive()));
    }

    @Test
    public void test_Template_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TEMPLATE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(templateVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getId());
        Assertions.assertEquals(templateVo1.getName(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getName());
        Assertions.assertEquals(templateVo1.getDescription(), om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getDescription());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), TemplateVo.class).getActive()));
    }

    @Test
    public void test_Template_Delete_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TEMPLATE_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Delete_ShouldReturn_422Response_And_ErrorCode_RES_PRINT_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Delete_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = templateEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Template_Delete_ShouldReturn_404Response_And_ErrorCode_RES_PRINT_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTemplateDetails() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        templateForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Template_Put_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenUpdatedBy_EmptyInvalidId_AndTemplateDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Put_ShouldReturn_404Response_And_ErrorCode_RES_PRINT_002_WhenUpdated_ByAbsentId_AndTemplateDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Template_Put_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_005_WhenUpdated_ByInactiveId_AndTemplateDetails() throws Exception {
        String id = templateEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Template_Put_ShouldReturn_422Response_And_ErrorCode_RES_PRINT_003_WhenUpdated_ById_AndNoTemplateDetails() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
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
    public void test_Template_Put_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        templateForm.setName("");

        mvcResult = mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Put_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidDescription() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "description";
        templateForm.setDescription("");

        mvcResult = mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Template_Put_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidTemplateTypeId(String templateTypeId) throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "templateTypeId";
        templateForm.setTemplateTypeId(templateTypeId);

        mvcResult = mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r ${}" })
    public void test_Template_Put_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidContent(String content) throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "content";
        templateForm.setContent(content);

        mvcResult = mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(templateForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Template_Put_ShouldReturn_422Response_And_ErrorCode_RES_PRINT_003_WhenUpdated_ById_AndEmptyTemplateDetails() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(TEMPLATE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new TemplateForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Template_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTemplateDetails() throws Exception {
        String id = templateEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Template_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndTemplateDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TEMPLATE_URI_BY_ID, " ")
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
    public void test_Template_Patch_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_002_WhenUpdated_ByInvalidId_AndTemplateDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
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
    public void test_Template_Patch_ShouldReturn_404Response_And_ErrorCode_RES_PRINT_002_WhenUpdated_ByAbsentId_AndTemplateDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
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
    public void test_Template_Patch_ShouldReturn_422Response_And_ErrorCode_RES_PRINT_003_WhenUpdated_ById_AndNoTemplateDetails() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Template_Patch_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
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
    public void test_Template_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
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
    public void test_Template_Patch_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidTemplateTypeId() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "templateTypeId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, "r"));

        mvcResult = mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
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
    public void test_Template_Patch_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidContent() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "content";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/content", "r ${}"));

        mvcResult = mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
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
    public void test_Template_Patch_ShouldReturn_400Response_And_ErrorCode_RES_PRINT_001_WhenRequested_ById_AndInvalidDefinitionOfTemplateAttribute() throws Exception {
        String id = templateEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = PrintErrorCode.PRINT_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(TEMPLATE_URI_BY_ID, id)
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
