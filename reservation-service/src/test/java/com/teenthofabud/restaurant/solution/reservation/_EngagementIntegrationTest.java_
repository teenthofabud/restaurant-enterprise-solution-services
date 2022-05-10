package com.teenthofabud.restaurant.solution.reservation;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.repository.BookingRepository;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementEvent;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementVo;
import com.teenthofabud.restaurant.solution.reservation.engagement.repository.EngagementRepository;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.reservation.integration.customer.data.AccountVo;
import com.teenthofabud.restaurant.solution.reservation.integration.establishmentarea.data.TableVo;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
public class EngagementIntegrationTest extends ReservationIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ENGAGEMENT_URI = "/engagement";
    private static final String ENGAGEMENT_URI_BY_ID = "/engagement/{id}";
    private static final String ENGAGEMENT_URI_BY_ASSOCIATION_ID = "/engagement/associationid/{associationId}";
    private static final String ENGAGEMENT_URI_FILTER = "/engagement/filter";

    private EngagementRepository engagementRepository;
    private CategoryRepository categoryRepository;
    private BookingRepository bookingRepository;

    private Integer bookingIntegrationPort;
    private String dateFormat;
    private String timeFormat;

    @Value("${res.reservation.engagement.date.format}")
    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Value("${res.reservation.engagement.time.format}")
    public void setTimeFormat(String timeFormat) {
        this.timeFormat = timeFormat;
    }


    @Value("${reservation.integration.port}")
    public void setBookingIntegrationPort(Integer bookingIntegrationPort) {
        this.bookingIntegrationPort = bookingIntegrationPort;
    }

    @Autowired
    public void setEngagementRepository(EngagementRepository engagementRepository) {
        this.engagementRepository = engagementRepository;
    }

    @Autowired
    public void setExperienceRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    @Autowired
    public void setAssociationRepository(BookingRepository bookingRepository) {
        this.bookingRepository = bookingRepository;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo4;
    private AccountVo accountVo22;

    private TableVo tableVo1;
    private TableVo tableVo2;
    private TableVo tableVo4;
    private TableVo tableVo22;

    private CategoryVo categoryVo1;
    private CategoryVo categoryVo2;
    private CategoryVo categoryVo3;
    private CategoryVo categoryVo4;
    private CategoryDocument categoryDocument1;
    private CategoryDocument categoryDocument2;
    private CategoryDocument categoryDocument3;
    private CategoryDocument categoryDocument4;

    private BookingVo bookingVo1;
    private BookingVo bookingVo2;
    private BookingVo bookingVo3;
    private BookingVo bookingVo4;
    private BookingVo bookingVo5;
    private BookingDocument bookingDocument1;
    private BookingDocument bookingDocument2;
    private BookingDocument bookingDocument3;
    private BookingDocument bookingDocument4;
    private BookingDocument bookingDocument5;

    private EngagementForm engagementForm;
    private EngagementVo engagementVo1;
    private EngagementVo engagementVo2;
    private EngagementVo engagementVo3;
    private EngagementVo engagementVo4;
    private EngagementVo engagementVo5;
    private EngagementVo engagementVo6;
    private EngagementVo engagementVo7;
    private EngagementVo engagementVo8;
    private EngagementVo engagementVo9;
    private EngagementVo engagementVo10;
    private EngagementVo engagementVo11;
    private EngagementDocument engagementDocument1;
    private EngagementDocument engagementDocument2;
    private EngagementDocument engagementDocument3;
    private EngagementDocument engagementDocument4;
    private EngagementDocument engagementDocument5;
    private EngagementDocument engagementDocument6;
    private EngagementDocument engagementDocument7;
    private EngagementDocument engagementDocument8;
    private EngagementDocument engagementDocument9;
    private EngagementDocument engagementDocument10;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Experience
         */

        categoryDocument1 = new CategoryDocument();
        categoryDocument1.setName("Experience 1 Name");
        categoryDocument1.setDescription("Experience 1 Event");
        categoryDocument1.setActive(Boolean.TRUE);

        categoryDocument1 = categoryRepository.save(categoryDocument1);

        categoryVo1 = new CategoryVo();
        categoryVo1.setId(categoryDocument1.getId());
        categoryVo1.setName(categoryDocument1.getName());
        categoryVo1.setDescription(categoryDocument1.getDescription());

        categoryDocument2 = new CategoryDocument();
        categoryDocument2.setName("Experience 2 Name");
        categoryDocument2.setDescription("Experience 2 Event");
        categoryDocument2.setActive(Boolean.FALSE);

        categoryDocument2 = categoryRepository.save(categoryDocument2);

        categoryVo2 = new CategoryVo();
        categoryVo2.setId(categoryDocument2.getId().toString());
        categoryVo2.setName(categoryDocument2.getName());
        categoryVo2.setDescription(categoryDocument2.getDescription());

        categoryDocument3 = new CategoryDocument();
        categoryDocument3.setName("Experience 3 Name");
        categoryDocument3.setDescription("Experience 3 Event");
        categoryDocument3.setActive(Boolean.TRUE);

        categoryDocument3 = categoryRepository.save(categoryDocument3);

        categoryVo3 = new CategoryVo();
        categoryVo3.setId(categoryDocument3.getId().toString());
        categoryVo3.setName(categoryDocument3.getName());
        categoryVo3.setDescription(categoryDocument3.getDescription());

        categoryDocument4 = new CategoryDocument();
        categoryDocument4.setName("Experience 4 Name");
        categoryDocument4.setDescription("Experience 4 Event");
        categoryDocument4.setActive(Boolean.TRUE);

        categoryDocument4 = categoryRepository.save(categoryDocument4);

        categoryVo4 = new CategoryVo();
        categoryVo4.setId(categoryDocument4.getId());
        categoryVo4.setName(categoryDocument4.getName());
        categoryVo4.setDescription(categoryDocument4.getDescription());

        /**
         * Account
         */

        accountVo1 = new AccountVo();
        accountVo1.setFirstName("Account 1");
        accountVo1.setLastName("Account 1");
        accountVo1.setActive(Boolean.TRUE);
        accountVo1.setId("1");

        accountVo2 = new AccountVo();
        accountVo2.setFirstName("Account 2");
        accountVo2.setLastName("Account 2");
        accountVo2.setActive(Boolean.TRUE);
        accountVo2.setId("2");

        accountVo4 = new AccountVo();
        accountVo4.setFirstName("Account 4");
        accountVo4.setLastName("Account 4");
        accountVo4.setActive(Boolean.TRUE);
        accountVo4.setId("4");

        accountVo22 = new AccountVo();
        accountVo22.setFirstName("Account 22");
        accountVo22.setLastName("Account 22");
        accountVo22.setActive(Boolean.FALSE);
        accountVo22.setId("22");

        /**
         * Table
         */

        tableVo1 = new TableVo();
        tableVo1.setTableId("1");
        tableVo1.setTableName("Table 1");
        tableVo1.setActive(Boolean.TRUE);

        tableVo2 = new TableVo();
        tableVo2.setTableId("2");
        tableVo2.setTableName("Table 2");
        tableVo2.setActive(Boolean.TRUE);

        tableVo4 = new TableVo();
        tableVo4.setTableId("4");
        tableVo4.setTableName("Table 4");
        tableVo4.setActive(Boolean.TRUE);

        tableVo22 = new TableVo();
        tableVo22.setTableId("22");
        tableVo22.setTableName("Table 22");
        tableVo22.setActive(Boolean.FALSE);

        /**
         * Association
         */

        bookingDocument1 = new BookingDocument();
        bookingDocument1.setAccountId("1");
        bookingDocument1.setTableId("1");
        bookingDocument1.setActive(Boolean.TRUE);
        bookingDocument1.setExperienceId(categoryDocument1.getId());

        bookingDocument1 = bookingRepository.save(bookingDocument1);

        bookingVo1 = new BookingVo();
        bookingVo1.setId(bookingDocument1.getId());
        bookingVo1.setExperienceId(categoryDocument1.getId());
        bookingVo1.setTableId(bookingDocument1.getTableId());
        bookingVo1.setAccountId(bookingDocument1.getAccountId());

        bookingDocument2 = new BookingDocument();
        bookingDocument2.setAccountId("1");
        bookingDocument2.setTableId("2");
        bookingDocument2.setActive(Boolean.FALSE);
        bookingDocument2.setExperienceId(categoryDocument2.getId());

        bookingDocument2 = bookingRepository.save(bookingDocument2);

        bookingVo2 = new BookingVo();
        bookingVo2.setId(bookingDocument2.getId());
        bookingVo2.setExperienceId(categoryDocument2.getId());
        bookingVo2.setTableId(bookingDocument2.getTableId());
        bookingVo2.setAccountId(bookingDocument2.getAccountId());

        bookingDocument3 = new BookingDocument();
        bookingDocument3.setAccountId("2");
        bookingDocument3.setTableId("1");
        bookingDocument3.setActive(Boolean.TRUE);
        bookingDocument3.setExperienceId(categoryDocument3.getId());

        bookingDocument3 = bookingRepository.save(bookingDocument3);

        bookingVo3 = new BookingVo();
        bookingVo3.setId(bookingDocument3.getId());
        bookingVo3.setExperienceId(categoryDocument3.getId());
        bookingVo3.setTableId(bookingDocument3.getTableId());
        bookingVo3.setAccountId(bookingDocument3.getAccountId());

        bookingDocument4 = new BookingDocument();
        bookingDocument4.setAccountId("2");
        bookingDocument4.setTableId("4");
        bookingDocument4.setActive(Boolean.TRUE);
        bookingDocument4.setExperienceId(categoryDocument4.getId());

        bookingDocument4 = bookingRepository.save(bookingDocument4);

        bookingVo4 = new BookingVo();
        bookingVo4.setId(bookingDocument4.getId());
        bookingVo4.setExperienceId(categoryDocument4.getId());
        bookingVo4.setTableId(bookingDocument4.getTableId());
        bookingVo4.setAccountId(bookingDocument4.getAccountId());

        bookingDocument5 = new BookingDocument();
        bookingDocument5.setAccountId("4");
        bookingDocument5.setTableId("2");
        bookingDocument5.setActive(Boolean.TRUE);
        bookingDocument5.setExperienceId(categoryDocument4.getId());

        bookingDocument5 = bookingRepository.save(bookingDocument5);

        bookingVo5 = new BookingVo();
        bookingVo5.setId(bookingDocument5.getId());
        bookingVo5.setExperienceId(categoryDocument4.getId());
        bookingVo5.setTableId(bookingDocument5.getTableId());
        bookingVo5.setAccountId(bookingDocument5.getAccountId());

        /**
         * Engagement
         */

        engagementForm = new EngagementForm();
        engagementForm.setEvent(EngagementEvent.END.name());
        engagementForm.setDate(LocalDate.now().format(DateTimeFormatter.ofPattern(dateFormat)));
        engagementForm.setTime(LocalTime.now().format(DateTimeFormatter.ofPattern(timeFormat)));
        engagementForm.setAssociationId(bookingDocument5.getId());

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/associationId", bookingDocument5.getId()),
                new PatchOperationForm("replace", "/event", EngagementEvent.END.name()));

        engagementDocument1 = new EngagementDocument();
        engagementDocument1.setAssociationId(bookingDocument1.getId());
        engagementDocument1.setDate(LocalDate.now());
        engagementDocument1.setTime(LocalTime.now());
        engagementDocument1.setActive(Boolean.TRUE);
        engagementDocument1.setEvent(EngagementEvent.START);

        engagementDocument1 = engagementRepository.save(engagementDocument1);

        engagementVo1 = new EngagementVo();
        engagementVo1.setId(engagementDocument1.getId());
        engagementVo1.setDate(engagementDocument1.getDate());
        engagementVo1.setTime(engagementDocument1.getTime());
        engagementVo1.setEvent(engagementDocument1.getEvent());
        engagementVo1.setAssociationId(engagementDocument1.getAssociationId());

        engagementDocument2 = new EngagementDocument();
        engagementDocument2.setAssociationId(bookingDocument1.getId());
        engagementDocument2.setDate(LocalDate.now());
        engagementDocument2.setTime(LocalTime.now().plusMinutes(2));
        engagementDocument2.setActive(Boolean.TRUE);
        engagementDocument2.setEvent(EngagementEvent.END);

        engagementDocument2 = engagementRepository.save(engagementDocument2);

        engagementVo2 = new EngagementVo();
        engagementVo2.setId(engagementDocument2.getId());
        engagementVo2.setDate(engagementDocument2.getDate());
        engagementVo2.setTime(engagementDocument2.getTime());
        engagementVo2.setEvent(engagementDocument2.getEvent());
        engagementVo2.setAssociationId(engagementDocument2.getAssociationId());

        engagementDocument3 = new EngagementDocument();
        engagementDocument3.setAssociationId(bookingDocument2.getId());
        engagementDocument3.setDate(LocalDate.now());
        engagementDocument3.setTime(LocalTime.now());
        engagementDocument3.setActive(Boolean.TRUE);
        engagementDocument3.setEvent(EngagementEvent.START);

        engagementDocument3 = engagementRepository.save(engagementDocument3);

        engagementVo3 = new EngagementVo();
        engagementVo3.setId(engagementDocument3.getId());
        engagementVo3.setDate(engagementDocument3.getDate());
        engagementVo3.setTime(engagementDocument3.getTime());
        engagementVo3.setEvent(engagementDocument3.getEvent());
        engagementVo3.setAssociationId(engagementDocument3.getAssociationId());

        engagementDocument4 = new EngagementDocument();
        engagementDocument4.setAssociationId(bookingDocument2.getId());
        engagementDocument4.setDate(LocalDate.now());
        engagementDocument4.setTime(LocalTime.now().plusMinutes(2));
        engagementDocument4.setActive(Boolean.TRUE);
        engagementDocument4.setEvent(EngagementEvent.END);

        engagementDocument4 = engagementRepository.save(engagementDocument4);

        engagementVo4 = new EngagementVo();
        engagementVo4.setId(engagementDocument4.getId());
        engagementVo4.setDate(engagementDocument4.getDate());
        engagementVo4.setTime(engagementDocument4.getTime());
        engagementVo4.setEvent(engagementDocument4.getEvent());
        engagementVo4.setAssociationId(engagementDocument4.getAssociationId());

        engagementDocument5 = new EngagementDocument();
        engagementDocument5.setAssociationId(bookingDocument3.getId());
        engagementDocument5.setDate(LocalDate.now());
        engagementDocument5.setTime(LocalTime.now());
        engagementDocument5.setActive(Boolean.TRUE);
        engagementDocument5.setEvent(EngagementEvent.START);

        engagementDocument5 = engagementRepository.save(engagementDocument5);

        engagementVo5 = new EngagementVo();
        engagementVo5.setId(engagementDocument5.getId());
        engagementVo5.setDate(engagementDocument5.getDate());
        engagementVo5.setTime(engagementDocument5.getTime());
        engagementVo5.setEvent(engagementDocument5.getEvent());
        engagementVo5.setAssociationId(engagementDocument5.getAssociationId());

        engagementDocument6 = new EngagementDocument();
        engagementDocument6.setAssociationId(bookingDocument3.getId());
        engagementDocument6.setDate(LocalDate.now());
        engagementDocument6.setTime(LocalTime.now().plusMinutes(2));
        engagementDocument6.setActive(Boolean.TRUE);
        engagementDocument6.setEvent(EngagementEvent.END);

        engagementDocument6 = engagementRepository.save(engagementDocument6);

        engagementVo6 = new EngagementVo();
        engagementVo6.setId(engagementDocument6.getId());
        engagementVo6.setDate(engagementDocument6.getDate());
        engagementVo6.setTime(engagementDocument6.getTime());
        engagementVo6.setEvent(engagementDocument6.getEvent());
        engagementVo6.setAssociationId(engagementDocument6.getAssociationId());

        engagementDocument7 = new EngagementDocument();
        engagementDocument7.setAssociationId(bookingDocument4.getId());
        engagementDocument7.setDate(LocalDate.now());
        engagementDocument7.setTime(LocalTime.now());
        engagementDocument7.setActive(Boolean.TRUE);
        engagementDocument7.setEvent(EngagementEvent.START);

        engagementDocument7 = engagementRepository.save(engagementDocument7);

        engagementVo7 = new EngagementVo();
        engagementVo7.setId(engagementDocument7.getId());
        engagementVo7.setDate(engagementDocument7.getDate());
        engagementVo7.setTime(engagementDocument7.getTime());
        engagementVo7.setEvent(engagementDocument7.getEvent());
        engagementVo7.setAssociationId(engagementDocument7.getAssociationId());

        engagementDocument8 = new EngagementDocument();
        engagementDocument8.setAssociationId(bookingDocument4.getId());
        engagementDocument8.setDate(LocalDate.now());
        engagementDocument8.setTime(LocalTime.now().plusMinutes(2));
        engagementDocument8.setActive(Boolean.TRUE);
        engagementDocument8.setEvent(EngagementEvent.END);

        engagementDocument8 = engagementRepository.save(engagementDocument8);

        engagementVo8 = new EngagementVo();
        engagementVo8.setId(engagementDocument8.getId());
        engagementVo8.setDate(engagementDocument8.getDate());
        engagementVo8.setTime(engagementDocument8.getTime());
        engagementVo8.setEvent(engagementDocument8.getEvent());
        engagementVo8.setAssociationId(engagementDocument8.getAssociationId());

        engagementDocument9 = new EngagementDocument();
        engagementDocument9.setAssociationId(bookingDocument5.getId());
        engagementDocument9.setDate(LocalDate.now());
        engagementDocument9.setTime(LocalTime.now().plusMinutes(2));
        engagementDocument9.setActive(Boolean.TRUE);
        engagementDocument9.setEvent(EngagementEvent.END);

        engagementDocument9 = engagementRepository.save(engagementDocument9);

        engagementVo9 = new EngagementVo();
        engagementVo9.setId(engagementDocument9.getId());
        engagementVo9.setDate(engagementDocument9.getDate());
        engagementVo9.setTime(engagementDocument9.getTime());
        engagementVo9.setEvent(engagementDocument9.getEvent());
        engagementVo9.setAssociationId(engagementDocument9.getAssociationId());

    }

    @AfterEach
    private void destroy() {
        engagementRepository.deleteById(engagementDocument1.getId());
        engagementRepository.deleteById(engagementDocument2.getId());
        engagementRepository.deleteById(engagementDocument3.getId());
        engagementRepository.deleteById(engagementDocument4.getId());
        engagementRepository.deleteById(engagementDocument5.getId());
        engagementRepository.deleteById(engagementDocument6.getId());
        engagementRepository.deleteById(engagementDocument7.getId());
        engagementRepository.deleteById(engagementDocument8.getId());
        engagementRepository.deleteById(engagementDocument9.getId());

        bookingRepository.deleteById(bookingDocument1.getId());
        bookingRepository.deleteById(bookingDocument2.getId());
        bookingRepository.deleteById(bookingDocument3.getId());
        bookingRepository.deleteById(bookingDocument4.getId());
        bookingRepository.deleteById(bookingDocument5.getId());

        categoryRepository.deleteById(categoryDocument1.getId());
        categoryRepository.deleteById(categoryDocument2.getId());
        categoryRepository.deleteById(categoryDocument3.getId());
        categoryRepository.deleteById(categoryDocument4.getId());
    }

    @Test
    public void test_Engagement_Post_ShouldReturn_201Response_And_NewEngagementId_WhenPosted_WithValidEngagementForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ENGAGEMENT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }
    
    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithEmptyAssociationId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "associationId";
        engagementForm.setAssociationId("");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithEmptyExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        engagementForm.setExperienceId("");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithEmptyEvent() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "event";
        engagementForm.setEvent("");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInactiveAssociationId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "associationId";
        engagementForm.setAssociationId("22");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInactiveExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        engagementForm.setExperienceId(categoryDocument2.getId().toString());

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInactiveEvent() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "event";
        engagementForm.setEvent("22");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithInvalidAssociationId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String fieldName = "invalid";
        engagementForm.setAssociationId("r");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithInvalidExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        engagementForm.setExperienceId("r");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_WithInvalidEvent() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-001";
        String fieldName = "invalid";
        engagementForm.setEvent("r");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_002_WhenRequested_WithAbsentAssociationId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-002";
        String fieldName = "unavailable";
        engagementForm.setAssociationId("3");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_WithAbsentExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        engagementForm.setExperienceId("99999");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_500Response_And_ErrorCode_RES_EREA_002_WhenRequested_WithAbsentEvent() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-002";
        String fieldName = "unavailable";
        engagementForm.setEvent("3");

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Engagement_Post_ShouldReturn_409Response_And_ErrorCode_RES_BOOKING_004_WhenRequested_WithDuplicateEngagement() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_EXISTS.getErrorCode();
        String field1Name = "associationId";
        String field2Name = "event";
        String field1Value = engagementDocument1.getAssociationId();
        String field2Value = engagementDocument1.getEvent();
        engagementForm.setAssociationId(field1Value);
        engagementForm.setEvent(field2Value);

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(engagementForm)))
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
    public void test_Engagement_Post_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenPosted_WithNoEngagementForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ENGAGEMENT_URI)
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
    public void test_Engagement_Get_ShouldReturn_200Response_And_EngagementListNaturallyOrdered_WhenRequested_ForAllEngagements() throws Exception {
        MvcResult mvcResult = null;
        List<EngagementVo> engagementList = new ArrayList<>(Arrays.asList(engagementVo6, engagementVo1, engagementVo2, engagementVo3, engagementVo5, engagementVo4));

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(engagementList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_EngagementListNaturallyOrdered_WhenRequested_ForEngagements_ByExperienceId() throws Exception {
        MvcResult mvcResult = null;
        engagementVo4.setExperience(null);
        engagementVo5.setExperience(null);
        List<EngagementVo> engagementList = Arrays.asList(engagementVo4, engagementVo5);

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ASSOCIATION_ID, categoryDocument4.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(engagementList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(engagementList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_EngagementListNaturallyOrdered_WhenRequested_ForEngagements_ByEmptyExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ASSOCIATION_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ByAbsentExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String experienceId = "kk";
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ASSOCIATION_ID, experienceId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(experienceId));
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ByInactiveExperienceId() throws Exception {
        MvcResult mvcResult = null;
        String experienceId = categoryDocument2.getId();
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String errorName = "invalid";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ASSOCIATION_ID, experienceId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(errorName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(experienceId));
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_EngagementListNaturallyOrdered_WhenRequested_ForEngagements_WithAssociationId() throws Exception {
        MvcResult mvcResult = null;
        List<EngagementVo> engagementList = new ArrayList<>(Arrays.asList(engagementVo1, engagementVo2));

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER)
                        .queryParam("associationId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(engagementList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(engagementList), mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Engagement_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyAssociationIdOnly(String associationId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER).queryParam("associationId", associationId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "22" })
    public void test_Engagement_Get_ShouldReturn_200Response_And_EmptyEngagementList_WhenRequestedBy_InactiveAssociationIdOnly(String associationId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER).queryParam("associationId", associationId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Engagement_Get_ShouldReturn_200Response_And_EmptyEngagementList_WhenRequestedBy_AbsentAssociationIdOnly(String associationId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER).queryParam("associationId", associationId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_EngagementListNaturallyOrdered_WhenRequested_ForEngagements_WithEvent() throws Exception {
        MvcResult mvcResult = null;
        engagementVo1.setExperience(null);
        List<EngagementVo> engagementList = new ArrayList<>(Arrays.asList(engagementVo1, engagementVo3));

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER)
                        .queryParam("event", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(engagementList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(engagementList), mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Engagement_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyEventOnly(String event) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER).queryParam("event", event))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "22" })
    public void test_Engagement_Get_ShouldReturn_200Response_And_EmptyEngagementList_WhenRequestedBy_InactiveEventOnly(String event) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER).queryParam("event", event))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Engagement_Get_ShouldReturn_200Response_And_EmptyEngagementList_WhenRequestedBy_AbsentEventOnly(String event) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER).queryParam("event", event))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001__WhenRequestedBy_UnsupportedFilterAttribute() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER).queryParam("experienceId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_EngagementListNaturallyOrdered_WhenRequested_ForEngagements_WithAssociationIdAndEvent() throws Exception {
        MvcResult mvcResult = null;
        engagementVo3.setExperience(null);
        engagementVo2.setExperience(null);
        engagementVo4.setExperience(null);
        engagementVo1.setExperience(null);
        engagementVo5.setExperience(null);
        List<EngagementVo> engagementList = Arrays.asList(engagementVo1);

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_FILTER)
                        .queryParam("associationId", "1")
                        .queryParam("event", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(engagementList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(engagementList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_EngagementDetails_WhenRequested_ById() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        engagementVo1.setExperience(null);

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(engagementVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(engagementVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Engagement_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = engagementDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(engagementVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getId());
        Assertions.assertEquals(engagementVo1.getEvent(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getEvent());
        Assertions.assertEquals(engagementVo1.getAssociationId(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getAssociationId());
        Assertions.assertEquals(engagementVo1.getExperienceId(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getExperienceId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getActive()));
    }

    @Test
    public void test_Engagement_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = engagementDocument1.getId();
        engagementVo1.setAccount(accountVo1);
        engagementVo1.setTable(tableVo1);
        engagementVo1.setExperience(categoryVo1);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ENGAGEMENT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(engagementVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getTable() != null);
        Assertions.assertEquals(engagementVo1.getTable().getEvent(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getTable().getEvent());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getAccount() != null);
        Assertions.assertEquals(engagementVo1.getAccount().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getAccount().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getExperience() != null);
        Assertions.assertEquals(engagementVo1.getExperience().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getExperience().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), EngagementVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Engagement_Delete_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenDeleted_ByEmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Delete_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = engagementDocument2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Engagement_Delete_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndEngagementDetails() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        engagementForm.setAssociationId("1");
        engagementForm.setEvent("4");

        mvcResult = this.mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenUpdatedBy_EmptyId_AndEngagementDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ByAbsentId_AndEngagementDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_005_WhenUpdated_ByInactiveId_AndEngagementDetails() throws Exception {
        String id = engagementDocument2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndNoEngagementDetails() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /*@ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyName(String name) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        engagementForm.setName(name);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyAssociationId(String associationId) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";
        engagementForm.setAssociationId(associationId);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Engagement_Put_ShouldReturn_500Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidAssociationId(String associationId) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String fieldName = "id";
        String message = "invalid";
        engagementForm.setAssociationId(associationId);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyInvalidExperienceId(String experienceId) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        engagementForm.setExperienceId(experienceId);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndEmptyEvent(String event) throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "event";
        engagementForm.setEvent(event);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Engagement_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_ById_AndInvalidEvent(String event) throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-001";
        String fieldName = "id";
        String message = "invalid";
        engagementForm.setEvent(event);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Engagement_Put_ShouldReturn_500Response_And_ErrorCode_RES_CUST_002_WhenRequested_ById_AndAbsentAssociationId(String associationId) throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-002";
        String fieldName = "id";
        engagementForm.setAssociationId(associationId);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(associationId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999999" })
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndAbsentExperienceId(String experienceId) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        engagementForm.setExperienceId(experienceId);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Engagement_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_002_WhenRequested_ById_AndAbsentEvent(String event) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-002";
        String fieldName = "id";
        engagementForm.setEvent(event);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(event));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInactiveAssociationId() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String associationId = "22";
        String fieldName = "associationId";
        engagementForm.setAssociationId(associationId);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInactiveExperienceId() throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String experienceId = categoryDocument2.getId();
        String fieldName = "experienceId";
        engagementForm.setExperienceId(experienceId);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInactiveEvent() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String event = "22";
        String fieldName = "event";
        engagementForm.setEvent(event);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(engagementForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndEmptyEngagementDetails() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new EngagementForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Engagement_Put_ShouldReturn_409Response_And_ErrorCode_RES_BOOKING_004_WhenUpdated_ById_AndDuplicateEngagementDetails() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_EXISTS.getErrorCode();
        String field1Name = "associationId";
        String field2Name = "event";
        String field1Value = engagementDocument1.getAssociationId();
        String field2Value = engagementDocument1.getEvent();
        engagementForm.setAssociationId(field1Value);
        engagementForm.setEvent(field2Value);

        mvcResult = mockMvc.perform(put(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(engagementForm)))
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
    public void test_Engagement_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndEngagementDetails() throws Exception {
        String id = engagementDocument1.getId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/associationId", "4"),
                new PatchOperationForm("replace", "/event", "1"));
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndEngagementDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, " ")
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
    public void test_Engagement_Patch_ShouldReturn_404Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ByAbsentId_AndEngagementDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_409Response_And_ErrorCode_RES_BOOKING_002_WhenUpdated_ById_AndDuplicateEngagementDetails() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_EXISTS.getErrorCode();
        String field1Name = "associationId";
        String field2Name = "event";
        String field1Value = engagementDocument1.getAssociationId();
        String field2Value = engagementDocument1.getEvent();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value));


        mvcResult = this.mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_422Response_And_ErrorCode_RES_BOOKING_003_WhenUpdated_ById_AndNoEngagementDetails() throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@Test
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAssociationId() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/associationId", " "));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyExperienceId() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/experienceId", " "));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyEvent() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/event", " "));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAssociationId(String associationId) throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/associationId", associationId));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyExperienceId(String experienceId) throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/experienceId", experienceId));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyEvent(String event) throws Exception {
        String id = engagementDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/event", event));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidAssociationId(String associationId) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "associationId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, associationId));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidExperienceId(String experienceId) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "experienceId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, experienceId));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidEvent(String event) throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = BookingErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "event";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, event));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @Test
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveAssociationId() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String associationId = "22";
        String fieldName = "associationId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, associationId));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveExperienceId() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String experienceId = categoryDocument2.getId().toString();
        String fieldName = "experienceId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, experienceId));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveEvent() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String event = "22";
        String fieldName = "event";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, event));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
    public void test_Engagement_Patch_ShouldReturn_400Response_And_ErrorCode_RES_BOOKING_001_WhenRequested_ById_AndInvalidDefinitionOfEngagementAttribute() throws Exception {
        String id = engagementDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.BOOKING_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ENGAGEMENT_URI_BY_ID, id)
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
        return "simulation";
    }

    @Override
    public Integer getServicePort() throws UnsupportedOperationException {
        return this.bookingIntegrationPort;
    }

    @Override
    public String[] getSimulationFilePaths() throws UnsupportedOperationException {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation.json") };
    }
}
