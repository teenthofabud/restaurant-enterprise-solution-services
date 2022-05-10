package com.teenthofabud.restaurant.solution.reservation;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.reservation.error.ReservationErrorCode;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingForm;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.repository.BookingRepository;
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

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_CLASS)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
public class BookingIntegrationTest extends ReservationIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String BOOKING_URI = "/booking";
    private static final String BOOKING_URI_BY_ID = "/booking/{id}";
    private static final String BOOKING_URI_BY_EXPERIENCE_ID = "/booking/categoryid/{categoryId}";
    private static final String BOOKING_URI_FILTER = "/booking/filter";

    private BookingRepository bookingRepository;
    private CategoryRepository categoryRepository;

    private Integer bookingIntegrationPort;
    private String bookingTimeFormat;

    @Value("${reservation.integration.port}")
    public void setBookingIntegrationPort(Integer bookingIntegrationPort) {
        this.bookingIntegrationPort = bookingIntegrationPort;
    }

    @Value("${res.reservation.booking.timestamp}")
    public void setBookingTimeFormat(String bookingTimeFormat) {
        this.bookingTimeFormat = bookingTimeFormat;
    }

    @Autowired
    public void setBookingRepository(BookingRepository bookingRepository) {
        this.bookingRepository = bookingRepository;
    }

    @Autowired
    public void setCategoryRepository(CategoryRepository categoryRepository) {
        this.categoryRepository = categoryRepository;
    }

    private String inactiveCategoryId;

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo4;
    private AccountVo accountVo22;

    /*private TableVo tableVo1;
    private TableVo tableVo2;
    private TableVo tableVo4;
    private TableVo tableVo22;*/

    private CategoryVo categoryVo1;
    private CategoryVo categoryVo2;
    private CategoryVo categoryVo3;
    private CategoryVo categoryVo4;
    private CategoryDocument categoryDocument1;
    private CategoryDocument categoryDocument2;
    private CategoryDocument categoryDocument3;
    private CategoryDocument categoryDocument4;

    private BookingForm bookingForm;
    private BookingVo bookingVo1;
    private BookingVo bookingVo2;
    private BookingVo bookingVo3;
    private BookingVo bookingVo4;
    private BookingVo bookingVo5;
    private BookingVo bookingVo6;
    private BookingVo bookingVo7;
    private BookingVo bookingVo8;
    private BookingDocument bookingDocument1;
    private BookingDocument bookingDocument2;
    private BookingDocument bookingDocument3;
    private BookingDocument bookingDocument4;
    private BookingDocument bookingDocument5;
    private BookingDocument bookingDocument6;
    private BookingDocument bookingDocument7;

    private List<PatchOperationForm> patches;

    private LocalDateTime now;

    @BeforeEach
    private void init() {

        /**
         * Category
         */

        categoryDocument1 = new CategoryDocument();
        categoryDocument1.setName("Category 1 Name");
        categoryDocument1.setDescription("Category 1 TableId");
        categoryDocument1.setActive(Boolean.TRUE);

        categoryDocument1 = categoryRepository.save(categoryDocument1);

        categoryVo1 = new CategoryVo();
        categoryVo1.setId(categoryDocument1.getId());
        categoryVo1.setName(categoryDocument1.getName());
        categoryVo1.setDescription(categoryDocument1.getDescription());

        categoryDocument2 = new CategoryDocument();
        categoryDocument2.setName("Category 2 Name");
        categoryDocument2.setDescription("Category 2 TableId");
        categoryDocument2.setActive(Boolean.FALSE);

        categoryDocument2 = categoryRepository.save(categoryDocument2);
        inactiveCategoryId = categoryDocument2.getId();

        categoryVo2 = new CategoryVo();
        categoryVo2.setId(categoryDocument2.getId().toString());
        categoryVo2.setName(categoryDocument2.getName());
        categoryVo2.setDescription(categoryDocument2.getDescription());

        categoryDocument3 = new CategoryDocument();
        categoryDocument3.setName("Category 3 Name");
        categoryDocument3.setDescription("Category 3 TableId");
        categoryDocument3.setActive(Boolean.TRUE);

        categoryDocument3 = categoryRepository.save(categoryDocument3);

        categoryVo3 = new CategoryVo();
        categoryVo3.setId(categoryDocument3.getId().toString());
        categoryVo3.setName(categoryDocument3.getName());
        categoryVo3.setDescription(categoryDocument3.getDescription());

        categoryDocument4 = new CategoryDocument();
        categoryDocument4.setName("Category 4 Name");
        categoryDocument4.setDescription("Category 4 TableId");
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

        /*tableVo1 = new TableVo();
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
        tableVo22.setActive(Boolean.FALSE);*/

        /**
         * Booking
         */

        now = LocalDateTime.of(2022, 05, 10, 11,30,22).plusHours(1l);

        bookingForm = new BookingForm();
        //bookingForm.setTableId("4");
        bookingForm.setCategoryId(categoryDocument3.getId());
        bookingForm.setAccountId("4");
        bookingForm.setTimestamp(DateTimeFormatter.ofPattern(bookingTimeFormat).format(now));
        //bookingForm.setNoOfPerson(4);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"),
                new PatchOperationForm("replace", "/categoryId", categoryDocument1.getId()));

        bookingDocument1 = new BookingDocument();
        bookingDocument1.setAccountId("1");
        //bookingDocument1.setTableId("1");
        bookingDocument1.setActive(Boolean.TRUE);
        bookingDocument1.setCategoryId(categoryDocument1.getId());
        //bookingDocument1.setNoOfPerson(categoryDocument1.getId());
        bookingDocument1.setTimestamp(now);

        bookingDocument1 = bookingRepository.save(bookingDocument1);

        bookingVo1 = new BookingVo();
        bookingVo1.setId(bookingDocument1.getId());
        bookingVo1.setCategoryId(categoryDocument1.getId());
        //bookingVo1.setTableId(bookingDocument1.getTableId());
        bookingVo1.setAccountId(bookingDocument1.getAccountId());
        bookingVo1.setTimestamp(bookingDocument1.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));

        bookingDocument2 = new BookingDocument();
        bookingDocument2.setAccountId("1");
        //bookingDocument2.setTableId("2");
        bookingDocument2.setActive(Boolean.FALSE);
        bookingDocument2.setCategoryId(categoryDocument3.getId());
        bookingDocument2.setTimestamp(now.minusHours(2l));

        bookingDocument2 = bookingRepository.save(bookingDocument2);

        bookingVo2 = new BookingVo();
        bookingVo2.setId(bookingDocument2.getId());
        bookingVo2.setCategoryId(categoryDocument3.getId());
        //bookingVo2.setTableId(bookingDocument2.getTableId());
        bookingVo2.setAccountId(bookingDocument2.getAccountId());
        bookingVo2.setTimestamp(bookingDocument2.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));

        bookingDocument3 = new BookingDocument();
        bookingDocument3.setAccountId("1");
        //bookingDocument3.setTableId("1");
        bookingDocument3.setActive(Boolean.TRUE);
        bookingDocument3.setCategoryId(categoryDocument1.getId());
        bookingDocument3.setTimestamp(now.plusHours(2l));

        bookingDocument3 = bookingRepository.save(bookingDocument3);

        bookingVo3 = new BookingVo();
        bookingVo3.setId(bookingDocument3.getId());
        bookingVo3.setCategoryId(categoryDocument1.getId());
        //bookingVo3.setTableId(bookingDocument3.getTableId());
        bookingVo3.setAccountId(bookingDocument3.getAccountId());
        bookingVo3.setTimestamp(bookingDocument3.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));

        bookingDocument4 = new BookingDocument();
        bookingDocument4.setAccountId("1");
        //bookingDocument4.setTableId("4");
        bookingDocument4.setActive(Boolean.TRUE);
        bookingDocument4.setCategoryId(categoryDocument4.getId());
        bookingDocument4.setTimestamp(now);

        bookingDocument4 = bookingRepository.save(bookingDocument4);

        bookingVo4 = new BookingVo();
        bookingVo4.setId(bookingDocument4.getId());
        bookingVo4.setCategoryId(categoryDocument4.getId());
        //bookingVo4.setTableId(bookingDocument4.getTableId());
        bookingVo4.setAccountId(bookingDocument4.getAccountId());
        bookingVo4.setTimestamp(bookingDocument4.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));

        bookingDocument5 = new BookingDocument();
        bookingDocument5.setAccountId("2");
        //bookingDocument5.setTableId("2");
        bookingDocument5.setActive(Boolean.TRUE);
        bookingDocument5.setCategoryId(categoryDocument4.getId());
        bookingDocument5.setTimestamp(now.plusSeconds(30l));

        bookingDocument5 = bookingRepository.save(bookingDocument5);

        bookingVo5 = new BookingVo();
        bookingVo5.setId(bookingDocument5.getId());
        bookingVo5.setCategoryId(categoryDocument4.getId());
        //bookingVo5.setTableId(bookingDocument5.getTableId());
        bookingVo5.setAccountId(bookingDocument5.getAccountId());
        bookingVo5.setTimestamp(bookingDocument5.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));

        bookingDocument6 = new BookingDocument();
        bookingDocument6.setAccountId("2");
        //bookingDocument5.setTableId("2");
        bookingDocument6.setActive(Boolean.TRUE);
        bookingDocument6.setCategoryId(categoryDocument3.getId());
        bookingDocument6.setTimestamp(now);

        bookingDocument6 = bookingRepository.save(bookingDocument6);

        bookingVo6 = new BookingVo();
        bookingVo6.setId(bookingDocument6.getId());
        bookingVo6.setCategoryId(categoryDocument3.getId());
        //bookingVo5.setTableId(bookingDocument5.getTableId());
        bookingVo6.setAccountId(bookingDocument6.getAccountId());
        bookingVo6.setTimestamp(bookingDocument6.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));

        bookingDocument7 = new BookingDocument();
        bookingDocument7.setAccountId("22");
        //bookingDocument5.setTableId("2");
        bookingDocument7.setActive(Boolean.TRUE);
        bookingDocument7.setCategoryId(categoryDocument4.getId());
        bookingDocument7.setTimestamp(now);

        bookingDocument7 = bookingRepository.save(bookingDocument7);

        bookingVo7 = new BookingVo();
        bookingVo7.setId(bookingDocument7.getId());
        bookingVo7.setCategoryId(categoryDocument4.getId());
        //bookingVo5.setTableId(bookingDocument5.getTableId());
        bookingVo7.setAccountId(bookingDocument7.getAccountId());
        bookingVo7.setTimestamp(bookingDocument7.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat)));


        bookingVo8 = new BookingVo();
        bookingVo8.setId(UUID.randomUUID().toString());
        bookingVo8.setCategoryId(bookingForm.getCategoryId());
        //bookingVo9.setTableId(bookingForm.getTableId());
        bookingVo8.setAccountId(bookingForm.getAccountId());

    }

    @AfterEach
    private void destroy() {
        bookingRepository.deleteById(bookingDocument1.getId());
        bookingRepository.deleteById(bookingDocument2.getId());
        bookingRepository.deleteById(bookingDocument3.getId());
        bookingRepository.deleteById(bookingDocument4.getId());
        bookingRepository.deleteById(bookingDocument5.getId());
        bookingRepository.deleteById(bookingDocument6.getId());
        bookingRepository.deleteById(bookingDocument7.getId());

        categoryRepository.deleteById(categoryDocument1.getId());
        categoryRepository.deleteById(categoryDocument2.getId());
        categoryRepository.deleteById(categoryDocument3.getId());
        categoryRepository.deleteById(categoryDocument4.getId());
    }

    @Test
    public void test_Booking_Post_ShouldReturn_201Response_And_NewBookingId_WhenPosted_WithValidBookingForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(BOOKING_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }
    
    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithEmptyAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "accountId";
        bookingForm.setAccountId("");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithEmptyCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        bookingForm.setCategoryId("");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithEmptyTimestamp() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "timestamp";
        bookingForm.setTimestamp("");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithEmptyTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        bookingForm.setTableId("");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithInactiveAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "accountId";
        bookingForm.setAccountId("22");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithInactiveCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        bookingForm.setCategoryId(categoryDocument2.getId().toString());

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithPastTimestamp() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "timestamp";
        bookingForm.setTimestamp(DateTimeFormatter.ofPattern(bookingTimeFormat).format(LocalDateTime.now().minusHours(2l)));

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithInactiveTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        bookingForm.setTableId("22");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @Test
    public void test_Booking_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithInvalidAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String fieldName = "invalid";
        bookingForm.setAccountId("r");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithInvalidCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        bookingForm.setCategoryId("r");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Booking_Post_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_WithInvalidTimestamp() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "timestamp";
        String message = "invalid";
        bookingForm.setTimestamp("r");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@Test
    public void test_Booking_Post_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_WithInvalidTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-001";
        String fieldName = "invalid";
        bookingForm.setTableId("r");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @Test
    public void test_Booking_Post_ShouldReturn_500Response_And_ErrorCode_RES_CUST_002_WhenRequested_WithAbsentAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-002";
        String fieldName = "unavailable";
        bookingForm.setAccountId("3");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Booking_Post_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_WithAbsentCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        bookingForm.setCategoryId("99999");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    /*@Test
    public void test_Booking_Post_ShouldReturn_500Response_And_ErrorCode_RES_EREA_002_WhenRequested_WithAbsentTableId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-002";
        String fieldName = "unavailable";
        bookingForm.setTableId("3");

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }*/

    @Test
    public void test_Booking_Post_ShouldReturn_409Response_And_ErrorCode_RES_RESV_004_WhenRequested_WithDuplicateBooking() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "categoryId";
        String field3Name = "timestamp";
        //String field2Name = "tableId";
        String field1Value = bookingDocument1.getAccountId();
        String field2Value = bookingDocument1.getCategoryId();
        String field3Value = bookingDocument1.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        //String field2Value = bookingDocument1.getTableId();
        bookingForm.setAccountId(field1Value);
        bookingForm.setCategoryId(field2Value);
        bookingForm.setTimestamp(field3Value);
        //bookingForm.setTableId(field2Value);

        mvcResult = mockMvc.perform(post(BOOKING_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(bookingForm)))
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

    @Test
    public void test_Booking_Post_ShouldReturn_422Response_And_ErrorCode_RES_RESV_003_WhenPosted_WithNoBookingForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(BOOKING_URI)
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
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForAllBookings() throws Exception {
        MvcResult mvcResult = null;
        List<BookingVo> bookingList = new ArrayList<>(Arrays.asList(bookingVo1, bookingVo2, bookingVo3, bookingVo4, bookingVo5, bookingVo6, bookingVo7, bookingVo8));

        mvcResult = this.mockMvc.perform(get(BOOKING_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
    }

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_ByCategoryId() throws Exception {
        MvcResult mvcResult = null;
        bookingVo4.setCategory(null);
        bookingVo5.setCategory(null);
        List<BookingVo> bookingList = Arrays.asList(bookingVo4, bookingVo5, bookingVo7);

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_EXPERIENCE_ID, categoryDocument4.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_ByEmptyCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_EXPERIENCE_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Booking_Get_ShouldReturn_404Response_And_ErrorCode_RES_RESV_001_WhenRequested_ByAbsentCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String categoryId = "kk";
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_EXPERIENCE_ID, categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(categoryId));
    }

    @Test
    public void test_Booking_Get_ShouldReturn_404Response_And_ErrorCode_RES_RESV_001_WhenRequested_ByInactiveCategoryId() throws Exception {
        MvcResult mvcResult = null;
        String categoryId = categoryDocument2.getId();
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String errorName = "invalid";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_EXPERIENCE_ID, categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(errorName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(categoryId));
    }

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithAccountId() throws Exception {
        MvcResult mvcResult = null;
        List<BookingVo> bookingList = new ArrayList<>(Arrays.asList(bookingVo1, bookingVo2, bookingVo3, bookingVo4));

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("accountId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Booking_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequestedBy_EmptyAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "22" })
    public void test_Booking_Get_ShouldReturn_200Response_And_MatchingBookingList_WhenRequestedBy_InactiveAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        List<BookingVo> bookingList = new ArrayList<>(Arrays.asList(bookingVo7));

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Booking_Get_ShouldReturn_200Response_And_EmptyBookingList_WhenRequestedBy_AbsentAccountIdOnly(String accountId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("accountId", accountId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
    }

    /*@Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithTableId() throws Exception {
        MvcResult mvcResult = null;
        bookingVo1.setCategory(null);
        List<BookingVo> bookingList = new ArrayList<>(Arrays.asList(bookingVo1, bookingVo3));

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("tableId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }*/

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithCategoryId() throws Exception {
        MvcResult mvcResult = null;
        bookingVo1.setCategory(null);
        bookingVo3.setCategory(null);
        List<BookingVo> bookingList = new ArrayList<>(Arrays.asList(bookingVo1, bookingVo3));

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("categoryId", categoryDocument1.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    /*@ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Booking_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequestedBy_EmptyTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Booking_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequestedBy_EmptyCategoryIdOnly(String categoryId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("categoryId", categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@ParameterizedTest
    @ValueSource(strings = { "22" })
    public void test_Booking_Get_ShouldReturn_200Response_And_EmptyBookingList_WhenRequestedBy_InactiveTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
    }*/

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_EmptyBookingList_WhenRequestedBy_InactiveCategoryIdOnly() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";
        String categoryId = categoryDocument2.getId();

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("categoryId", categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
    }

    /*@ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Booking_Get_ShouldReturn_200Response_And_EmptyBookingList_WhenRequestedBy_AbsentTableIdOnly(String tableId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("tableId", tableId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
    }*/

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Booking_Get_ShouldReturn_200Response_And_EmptyBookingList_WhenRequestedBy_AbsentCategoryIdOnly(String categoryId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("categoryId", categoryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
    }

    @Test
    public void test_Booking_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001__WhenRequestedBy_UnsupportedFilterAttribute() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "timestamp";
        String message = "invalid";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER).queryParam("timestamp", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /*@Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithAccountIdAndTableId() throws Exception {
        MvcResult mvcResult = null;
        bookingVo3.setCategory(null);
        bookingVo2.setCategory(null);
        bookingVo4.setCategory(null);
        bookingVo1.setCategory(null);
        bookingVo5.setCategory(null);
        List<BookingVo> bookingList = Arrays.asList(bookingVo1);

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("accountId", "1")
                        .queryParam("tableId", "1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }*/

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithAccountIdAndCategoryId() throws Exception {
        MvcResult mvcResult = null;
        bookingVo1.setCategory(null);
        bookingVo3.setCategory(null);
        List<BookingVo> bookingList = Arrays.asList(bookingVo1, bookingVo3);

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("accountId", "1")
                        .queryParam("categoryId", categoryDocument1.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithAccountIdAndTimestamp() throws Exception {
        MvcResult mvcResult = null;
        bookingVo1.setCategory(null);
        bookingVo4.setCategory(null);
        //String timestamp = bookingDocument1.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        String timestamp = now.format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        List<BookingVo> bookingList = Arrays.asList(bookingVo1, bookingVo4);
        //List<BookingVo> bookingList = Arrays.asList(bookingVo1);
        //List<BookingVo> bookingList = Arrays.asList(bookingVo4);

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("accountId", "1")
                        .queryParam("timestamp", timestamp))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithCategoryIdAndTimestamp() throws Exception {
        MvcResult mvcResult = null;
        bookingVo4.setCategory(null);
        bookingVo7.setCategory(null);
        String timestamp = bookingDocument4.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        List<BookingVo> bookingList = Arrays.asList(bookingVo4, bookingVo7);

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("categoryId", categoryDocument4.getId())
                        .queryParam("timestamp", timestamp))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingListNaturallyOrdered_WhenRequested_ForBookings_WithAccountIdAndCategoryIdAndTimestamp() throws Exception {
        MvcResult mvcResult = null;
        bookingVo4.setCategory(null);
        bookingVo7.setCategory(null);
        String timestamp = bookingDocument4.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        List<BookingVo> bookingList = Arrays.asList(bookingVo4, bookingVo7);

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_FILTER)
                        .queryParam("categoryId", categoryDocument4.getId())
                        .queryParam("timestamp", timestamp))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(bookingList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_BookingDetails_WhenRequested_ById() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        bookingVo1.setCategory(null);

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(bookingVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(bookingVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Booking_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequestedBy_EmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Booking_Get_ShouldReturn_400Response_And_ErrorCode_RES_RESV_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Booking_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = bookingDocument4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    /*@Test
    public void test_Booking_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getId());
        Assertions.assertEquals(bookingVo1.getTableId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getTableId());
        Assertions.assertEquals(bookingVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getAccountId());
        Assertions.assertEquals(bookingVo1.getCategoryId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCategoryId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getActive()));
    }*/

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getId());
        Assertions.assertEquals(bookingVo1.getTimestamp(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getTimestamp());
        Assertions.assertEquals(bookingVo1.getAccountId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getAccountId());
        Assertions.assertEquals(bookingVo1.getCategoryId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCategoryId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getActive()));
    }

    /*@Test
    public void test_Booking_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = bookingDocument1.getId();
        bookingVo1.setAccount(accountVo1);
        bookingVo1.setTable(tableVo1);
        bookingVo1.setCategory(categoryVo1);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getTable() != null);
        Assertions.assertEquals(bookingVo1.getTable().getTableId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getTable().getTableId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getAccount() != null);
        Assertions.assertEquals(bookingVo1.getAccount().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getAccount().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCategory() != null);
        Assertions.assertEquals(bookingVo1.getCategory().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCategory().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getActive()));
    }*/

    @Test
    public void test_Booking_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = bookingDocument1.getId();
        bookingVo1.setAccount(accountVo1);
        //bookingVo1.setTable(tableVo1);
        bookingVo1.setCategory(categoryVo1);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(BOOKING_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(bookingVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getTimestamp() != null);
        Assertions.assertEquals(bookingVo1.getTimestamp(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getTimestamp());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getAccount() != null);
        Assertions.assertEquals(bookingVo1.getAccount().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getAccount().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCategory() != null);
        Assertions.assertEquals(bookingVo1.getCategory().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCategory().getId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), BookingVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Booking_Delete_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenDeleted_ByEmptyId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Booking_Delete_ShouldReturn_400Response_And_ErrorCode_RES_RESV_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = bookingDocument2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Booking_Delete_ShouldReturn_404Response_And_ErrorCode_RES_RESV_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@Test
    public void test_Booking_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        bookingForm.setAccountId("1");
        bookingForm.setTableId("4");

        mvcResult = this.mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }*/

    @Test
    public void test_Booking_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        bookingForm.setAccountId("1");
        bookingForm.setCategoryId(categoryDocument3.getId());

        mvcResult = this.mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " " })
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenUpdatedBy_EmptyId_AndBookingDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Booking_Put_ShouldReturn_404Response_And_ErrorCode_RES_RESV_002_WhenUpdated_ByAbsentId_AndBookingDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_005_WhenUpdated_ByInactiveId_AndBookingDetails() throws Exception {
        String id = bookingDocument2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Booking_Put_ShouldReturn_422Response_And_ErrorCode_RES_RESV_003_WhenUpdated_ById_AndNoBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndEmptyName(String name) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        bookingForm.setName(name);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndEmptyAccountId(String accountId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";
        bookingForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Booking_Put_ShouldReturn_500Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidAccountId(String accountId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-CUST-001";
        String fieldName = "id";
        String message = "invalid";
        bookingForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
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
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndEmptyInvalidCategoryId(String categoryId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        bookingForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndEmptyTableId(String tableId) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        bookingForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndEmptyTimestamp(String timestamp) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "timestamp";
        bookingForm.setTimestamp(timestamp);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Booking_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_ById_AndInvalidTableId(String tableId) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-001";
        String fieldName = "id";
        String message = "invalid";
        bookingForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }*/

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Booking_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_001_WhenRequested_ById_AndInvalidTimestamp(String timestamp) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";
        String message = "timestamp";
        bookingForm.setTimestamp(timestamp);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Booking_Put_ShouldReturn_500Response_And_ErrorCode_RES_CUST_002_WhenRequested_ById_AndAbsentAccountId(String accountId) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        //String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String errorCode = "RES-CUST-002";
        String fieldName = "id";
        String message = "unavailable";
        bookingForm.setAccountId(accountId);
        //bookingForm.setTimestamp(bookingDocument1.getTimestamp());

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(accountId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999999" })
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndAbsentCategoryId(String categoryId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        bookingForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@ParameterizedTest
    @ValueSource(strings = { "3" })
    public void test_Booking_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_002_WhenRequested_ById_AndAbsentTableId(String tableId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = "RES-EAREA-002";
        String fieldName = "id";
        bookingForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(tableId));
    }*/

    @Test
    public void test_Booking_Put_ShouldReturn_500Response_And_ErrorCode_RES_EAREA_002_WhenRequested_ById_AndPastTimestamp() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "timestamp";
        String timestamp = LocalDateTime.now().minusHours(2l).format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        bookingForm.setTimestamp(timestamp);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndInactiveAccountId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String accountId = "22";
        String fieldName = "accountId";
        bookingForm.setAccountId(accountId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndInactiveCategoryId() throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String categoryId = categoryDocument2.getId();
        String fieldName = "categoryId";
        bookingForm.setCategoryId(categoryId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@Test
    public void test_Booking_Put_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndInactiveTableId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String tableId = "22";
        String fieldName = "tableId";
        bookingForm.setTableId(tableId);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @Test
    public void test_Booking_Put_ShouldReturn_422Response_And_ErrorCode_RES_RESV_003_WhenUpdated_ById_AndEmptyBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new BookingForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /*@Test
    public void test_Booking_Put_ShouldReturn_409Response_And_ErrorCode_RES_RESV_004_WhenUpdated_ById_AndDuplicateBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "tableId";
        String field1Value = bookingDocument1.getAccountId();
        String field2Value = bookingDocument1.getTableId();
        bookingForm.setAccountId(field1Value);
        bookingForm.setTableId(field2Value);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(bookingForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Value));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Value));
    }*/

    @Test
    public void test_Booking_Put_ShouldReturn_409Response_And_ErrorCode_RES_RESV_004_WhenUpdated_ById_AndDuplicateBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "categoryId";
        String field3Name = "timestamp";
        String field1Value = bookingDocument1.getAccountId();
        String field2Value = bookingDocument1.getCategoryId();
        String field3Value = DateTimeFormatter.ofPattern(bookingTimeFormat).format(bookingDocument3.getTimestamp());
        bookingForm.setAccountId(field1Value);
        bookingForm.setCategoryId(field2Value);
        bookingForm.setTimestamp(field3Value);

        mvcResult = mockMvc.perform(put(BOOKING_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(bookingForm)))
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

    /*@Test
    public void test_Booking_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndBookingDetails() throws Exception {
        String id = bookingDocument1.getId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "4"),
                new PatchOperationForm("replace", "/tableId", "1"));
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }*/

    @Test
    public void test_Booking_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndBookingDetails() throws Exception {
        String id = bookingDocument1.getId();
        /*patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", "2"),
                new PatchOperationForm("replace", "/categoryId", "1"));*/
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndBookingDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(BOOKING_URI_BY_ID, " ")
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
    public void test_Booking_Patch_ShouldReturn_404Response_And_ErrorCode_RES_RESV_002_WhenUpdated_ByAbsentId_AndBookingDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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

    /*@Test
    public void test_Booking_Patch_ShouldReturn_409Response_And_ErrorCode_RES_RESV_002_WhenUpdated_ById_AndDuplicateBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "tableId";
        String field1Value = bookingDocument1.getAccountId();
        String field2Value = bookingDocument1.getTableId();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value));


        mvcResult = this.mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    }*/

    @Test
    public void test_Booking_Patch_ShouldReturn_409Response_And_ErrorCode_RES_RESV_002_WhenUpdated_ById_AndDuplicateBookingDetails() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_EXISTS.getErrorCode();
        String field1Name = "accountId";
        String field2Name = "categoryId";
        String field3Name = "timestamp";
        String field1Value = bookingDocument1.getAccountId();
        String field2Value = bookingDocument1.getCategoryId();
        String field3Value = bookingDocument1.getTimestamp().format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value),
                new PatchOperationForm("replace", "/" + field3Name, field3Value));


        mvcResult = this.mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field3Value));
    }

    @Test
    public void test_Booking_Patch_ShouldReturn_422Response_And_ErrorCode_RES_RESV_003_WhenUpdated_ById_AndNoBookingDetails() throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(BOOKING_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAccountId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", " "));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCategoryId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/categoryId", " "));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyTableId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableId", " "));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyAccountId(String accountId) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/accountId", accountId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyCategoryId(String categoryId) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/categoryId", categoryId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    @ValueSource(strings = { " ", "" })
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyTableId(String tableId) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/tableId", tableId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyTimestamp(String tableId) throws Exception {
        String id = bookingDocument1.getId();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/timestamp", tableId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidAccountId(String accountId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, accountId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
                        .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                        .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { "r" })
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidCategoryId(String categoryId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "categoryId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, categoryId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndInvalidTimestamp() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "timestamp";
        String timestamp = "r";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, timestamp));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidTableId(String tableId) throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "tableId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, tableId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveAccountId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String accountId = "22";
        String fieldName = "accountId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, accountId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveCategoryId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String categoryId = categoryDocument2.getId().toString();
        String fieldName = "categoryId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, categoryId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveTableId() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String tableId = "22";
        String fieldName = "tableId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, tableId));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndPastTimestamp() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String timestamp = LocalDateTime.now().minusHours(2l).format(DateTimeFormatter.ofPattern(bookingTimeFormat));
        String fieldName = "timestamp";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, timestamp));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
    public void test_Booking_Patch_ShouldReturn_400Response_And_ErrorCode_RES_RESV_001_WhenRequested_ById_AndInvalidDefinitionOfBookingAttribute() throws Exception {
        String id = bookingDocument1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = ReservationErrorCode.RESERVATION_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(BOOKING_URI_BY_ID, id)
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
