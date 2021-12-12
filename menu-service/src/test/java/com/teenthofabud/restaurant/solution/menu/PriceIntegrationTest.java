package com.teenthofabud.restaurant.solution.menu;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.item.repository.ItemRepository;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceEntity;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceForm;
import com.teenthofabud.restaurant.solution.menu.price.data.PriceVo;
import com.teenthofabud.restaurant.solution.menu.price.repository.PriceRepository;
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
public class PriceIntegrationTest extends MenuIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String PRICE_URI = "/price";
    private static final String PRICE_URI_BY_ID = "/price/{id}";
    private static final String PRICE_URI_BY_ITEM_ID = "/price/itemid/{itemId}";
    private static final String PRICE_URI_FILTER = "/price/filter";

    private PriceRepository priceRepository;
    private ItemRepository itemRepository;

    @Autowired
    public void setPriceRepository(PriceRepository priceRepository) {
        this.priceRepository = priceRepository;
    }

    @Autowired
    public void setItemRepository(ItemRepository itemRepository) {
        this.itemRepository = itemRepository;
    }

    private ItemVo itemVo1;
    private ItemVo itemVo2;
    private ItemVo itemVo3;
    private ItemVo itemVo4;
    private ItemEntity itemEntity1;
    private ItemEntity itemEntity2;
    private ItemEntity itemEntity3;
    private ItemEntity itemEntity4;

    private PriceForm priceForm;
    private PriceVo priceVo1;
    private PriceVo priceVo2;
    private PriceVo priceVo3;
    private PriceVo priceVo4;
    private PriceVo priceVo5;
    private PriceVo priceVo6;
    private PriceEntity priceEntity1;
    private PriceEntity priceEntity2;
    private PriceEntity priceEntity3;
    private PriceEntity priceEntity4;
    private PriceEntity priceEntity5;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        /**
         * Item
         */

        itemEntity1 = new ItemEntity();
        itemEntity1.setName("Item 1 Name");
        itemEntity1.setDescription("Item 1 Description");
        itemEntity1.setActive(Boolean.TRUE);

        itemEntity1 = itemRepository.save(itemEntity1);

        itemVo1 = new ItemVo();
        itemVo1.setId(itemEntity1.getId().toString());
        itemVo1.setName(itemEntity1.getName());
        itemVo1.setDescription(itemEntity1.getDescription());

        itemEntity2 = new ItemEntity();
        itemEntity2.setName("Item 2 Name");
        itemEntity2.setDescription("Item 2 Description");
        itemEntity2.setActive(Boolean.FALSE);

        itemEntity2 = itemRepository.save(itemEntity2);

        itemVo2 = new ItemVo();
        itemVo2.setId(itemEntity2.getId().toString());
        itemVo2.setName(itemEntity2.getName());
        itemVo2.setDescription(itemEntity2.getDescription());

        itemEntity3 = new ItemEntity();
        itemEntity3.setName("Item 3 Name");
        itemEntity3.setDescription("Item 3 Description");
        itemEntity3.setActive(Boolean.TRUE);

        itemEntity3 = itemRepository.save(itemEntity3);

        itemVo3 = new ItemVo();
        itemVo3.setId(itemEntity3.getId().toString());
        itemVo3.setName(itemEntity3.getName());
        itemVo3.setDescription(itemEntity3.getDescription());

        itemEntity4 = new ItemEntity();
        itemEntity4.setName("Item 4 Name");
        itemEntity4.setDescription("Item 4 Description");
        itemEntity4.setActive(Boolean.TRUE);

        itemEntity4 = itemRepository.save(itemEntity4);

        itemVo4 = new ItemVo();
        itemVo4.setId(itemEntity4.getId().toString());
        itemVo4.setName(itemEntity4.getName());
        itemVo4.setDescription(itemEntity4.getDescription());

        /**
         * Price
         */

        priceForm = new PriceForm();
        priceForm.setAmount(12.0);
        priceForm.setCurrencyId("INR");
        priceForm.setItemId(itemEntity3.getId().toString());

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/amount", "13"),
                new PatchOperationForm("replace", "/currencyId", "USD"),
                new PatchOperationForm("replace", "/itemId", itemEntity3.getId().toString()));

        priceEntity1 = new PriceEntity();
        priceEntity1.setAmount(22.0d);
        priceEntity1.setCurrencyId("INR");
        priceEntity1.setActive(Boolean.TRUE);
        priceEntity1.setItem(itemEntity1);

        priceEntity1 = priceRepository.save(priceEntity1);

        priceVo1 = new PriceVo();
        priceVo1.setId(priceEntity1.getId().toString());
        priceVo1.setAmount(priceEntity1.getAmount());
        priceVo1.setCurrency(Currency.getInstance(priceEntity1.getCurrencyId()));
        priceVo1.setItemId(priceEntity1.getItem().getId().toString());
        priceVo1.setItem(itemVo1);

        priceEntity2 = new PriceEntity();
        priceEntity2.setAmount(32.0d);
        priceEntity2.setCurrencyId("EUR");
        priceEntity2.setActive(Boolean.FALSE);
        priceEntity2.setItem(itemEntity2);

        priceEntity2 = priceRepository.save(priceEntity2);

        priceVo2 = new PriceVo();
        priceVo2.setId(priceEntity2.getId().toString());
        priceVo2.setAmount(priceEntity2.getAmount());
        priceVo2.setCurrency(Currency.getInstance(priceEntity2.getCurrencyId()));
        priceVo2.setItemId(priceEntity2.getItem().getId().toString());
        priceVo2.setItem(itemVo2);

        priceEntity3 = new PriceEntity();
        priceEntity3.setAmount(42.0d);
        priceEntity3.setCurrencyId("INR");
        priceEntity3.setActive(Boolean.TRUE);
        priceEntity3.setItem(itemEntity3);

        priceEntity3 = priceRepository.save(priceEntity3);

        priceVo3 = new PriceVo();
        priceVo3.setId(priceEntity3.getId().toString());
        priceVo3.setAmount(priceEntity3.getAmount());
        priceVo3.setCurrency(Currency.getInstance(priceEntity3.getCurrencyId()));
        priceVo3.setItemId(priceEntity3.getItem().getId().toString());
        priceVo3.setItem(itemVo3);

        priceEntity4 = new PriceEntity();
        priceEntity4.setAmount(52.0d);
        priceEntity4.setCurrencyId("EUR");
        priceEntity4.setActive(Boolean.TRUE);
        priceEntity4.setItem(itemEntity4);

        priceEntity4 = priceRepository.save(priceEntity4);

        priceVo4 = new PriceVo();
        priceVo4.setId(priceEntity4.getId().toString());
        priceVo4.setAmount(priceEntity4.getAmount());
        priceVo4.setCurrency(Currency.getInstance(priceEntity4.getCurrencyId()));
        priceVo4.setItemId(priceEntity4.getItem().getId().toString());
        priceVo4.setItem(itemVo4);

        priceEntity5 = new PriceEntity();
        priceEntity5.setAmount(442.0d);
        priceEntity5.setCurrencyId("INR");
        priceEntity5.setActive(Boolean.TRUE);
        priceEntity5.setItem(itemEntity4);

        priceEntity5 = priceRepository.save(priceEntity5);

        priceVo5 = new PriceVo();
        priceVo5.setId(priceEntity5.getId().toString());
        priceVo5.setAmount(priceEntity4.getAmount());
        priceVo5.setCurrency(Currency.getInstance(priceEntity4.getCurrencyId()));
        priceVo5.setItemId(priceEntity5.getItem().getId().toString());
        priceVo5.setItem(itemVo4);

        priceVo6 = new PriceVo();
        priceVo6.setId(UUID.randomUUID().toString());
        priceVo6.setAmount(priceForm.getAmount());
        priceVo6.setCurrency(Currency.getInstance(priceForm.getCurrencyId()));
        priceVo6.setItemId(priceForm.getItemId());
        priceVo6.setItem(itemVo3);

    }

    @AfterEach
    private void destroy() {
        priceEntity1.setItem(null);
        priceEntity2.setItem(null);
        priceEntity3.setItem(null);
        priceEntity4.setItem(null);
        priceEntity5.setItem(null);

        priceRepository.deleteById(priceEntity1.getId());
        priceRepository.deleteById(priceEntity2.getId());
        priceRepository.deleteById(priceEntity3.getId());
        priceRepository.deleteById(priceEntity4.getId());
        priceRepository.deleteById(priceEntity5.getId());

        itemRepository.deleteById(itemEntity1.getId());
        itemRepository.deleteById(itemEntity2.getId());
        itemRepository.deleteById(itemEntity3.getId());
        itemRepository.deleteById(itemEntity4.getId());
    }

    @Test
    public void test_Price_Post_ShouldReturn_201Response_And_NewPriceId_WhenPosted_WithValidPriceForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(PRICE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyAmount() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "amount";
        priceForm.setAmount(null);

        mvcResult = mockMvc.perform(post(PRICE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyCurrencyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "currencyId";
        priceForm.setCurrencyId("");

        mvcResult = mockMvc.perform(post(PRICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithEmptyItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        priceForm.setItemId("");

        mvcResult = mockMvc.perform(post(PRICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInactiveItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        priceForm.setItemId(itemEntity2.getId().toString());

        mvcResult = mockMvc.perform(post(PRICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInvalidAmount() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "amount";
        priceForm.setAmount(-9.0d);

        mvcResult = mockMvc.perform(post(PRICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInvalidCurrencyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "currencyId";
        priceForm.setCurrencyId("r");

        mvcResult = mockMvc.perform(post(PRICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithInvalidItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        priceForm.setItemId("r");

        mvcResult = mockMvc.perform(post(PRICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_WithAbsentItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        priceForm.setItemId(String.valueOf(Long.MAX_VALUE));

        mvcResult = mockMvc.perform(post(PRICE_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Price_Post_ShouldReturn_409Response_And_ErrorCode_RES_MENU_004_WhenRequested_WithDuplicatePrice() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_EXISTS.getErrorCode();
        String field1Name = "itemId";
        String field2Name = "currencyId";
        String field1Value = priceEntity1.getItem().getId().toString();
        String field2Value = priceEntity1.getCurrencyId();
        priceForm.setCurrencyId(field2Value);
        priceForm.setItemId(field1Value);

        mvcResult = mockMvc.perform(post(PRICE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
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
    public void test_Price_Post_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenPosted_WithNoPriceForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(PRICE_URI)
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
    public void test_Price_Get_ShouldReturn_200Response_And_PriceListNaturallyOrdered_WhenRequested_ForAllPrices() throws Exception {
        MvcResult mvcResult = null;
        List<PriceVo> priceList = new ArrayList<>(Arrays.asList(priceVo5, priceVo1));

        mvcResult = this.mockMvc.perform(get(PRICE_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(priceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo[].class).length);
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_PriceListNaturallyOrdered_WhenRequested_ForPrices_ByItemId() throws Exception {
        MvcResult mvcResult = null;
        priceVo4.setItem(null);
        priceVo5.setItem(null);
        List<PriceVo> priceList = Arrays.asList(priceVo4, priceVo5);

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ITEM_ID, itemEntity4.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(priceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(priceList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_PriceListNaturallyOrdered_WhenRequested_ForPrices_ByEmptyItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ITEM_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Get_ShouldReturn_404Response_And_ErrorCode_RES_MENU_001_WhenRequested_ByAbsentItemId() throws Exception {
        MvcResult mvcResult = null;
        String itemId = String.valueOf(Long.MAX_VALUE);
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ITEM_ID, itemId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(itemId));
    }

    @Test
    public void test_Price_Get_ShouldReturn_404Response_And_ErrorCode_RES_MENU_001_WhenRequested_ByInvalidItemId() throws Exception {
        MvcResult mvcResult = null;
        String itemId = "kk";
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ITEM_ID, itemId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(itemId));
    }

    @Test
    public void test_Price_Get_ShouldReturn_404Response_And_ErrorCode_RES_MENU_001_WhenRequested_ByInactiveItemId() throws Exception {
        MvcResult mvcResult = null;
        String itemId = itemEntity2.getId().toString();
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();
        String errorName = "deactivated";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ITEM_ID, itemId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(errorName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(itemId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Price_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyCurrencyIdOnly(String currencyId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER).queryParam("currencyId", currencyId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Price_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyItemIdOnly(String itemId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER).queryParam("itemId", itemId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }


    @Test
    public void test_Price_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001__WhenRequestedBy_UnsupportedFilterAttribute() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER).queryParam("imageUrl", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_InvalidCurrencyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "currencyId";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER).queryParam("currencyId", "r1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_InvalidItemId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER).queryParam("itemId", "r1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_PriceListNaturallyOrdered_WhenRequested_ForPrices_WithCurrencyId() throws Exception {
        MvcResult mvcResult = null;
        priceVo1.setItem(null);
        List<PriceVo> priceList = new ArrayList<>(Arrays.asList(priceVo1, priceVo3, priceVo5));

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER)
                        .queryParam("currencyId", "INR"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(priceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(priceList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_PriceListNaturallyOrdered_WhenRequested_ForPrices_WithItemId() throws Exception {
        MvcResult mvcResult = null;
        priceVo1.setItem(null);
        List<PriceVo> priceList = new ArrayList<>(Arrays.asList(priceVo1));

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER)
                        .queryParam("itemId", itemEntity1.getId().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(priceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(priceList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_PriceListNaturallyOrdered_WhenRequested_ForPrices_WithItemIdAndCurrencyId() throws Exception {
        MvcResult mvcResult = null;
        priceVo2.setItem(null);
        List<PriceVo> priceList = Arrays.asList(priceVo2);

        mvcResult = this.mockMvc.perform(get(PRICE_URI_FILTER)
                        .queryParam("itemId", itemEntity2.getId().toString())
                        .queryParam("currencyId", "EUR"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(priceList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(priceList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_PriceDetails_WhenRequested_ById() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        priceVo1.setItem(null);

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(priceVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(priceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Price_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Get_ShouldReturn_400Response_And_ErrorCode_RES_MENU_002_WhenRequested_ByAbsentId() throws Exception {
        String id = String.valueOf(Long.MAX_VALUE);
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = priceEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(priceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getId());
        Assertions.assertEquals(priceVo1.getCurrencyId(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getCurrencyId());
        Assertions.assertEquals(priceVo1.getItemId(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getItemId());
        Assertions.assertEquals(priceVo1.getAmount(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getAmount());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getActive()));
    }

    @Test
    public void test_Price_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(PRICE_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(priceVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getId());
        Assertions.assertEquals(priceVo1.getAmount(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getAmount());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getItem() != null);
        Assertions.assertEquals(priceVo1.getItem().getId(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getItem().getId());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getItem() != null);
        Assertions.assertEquals(priceVo1.getCurrency().getCurrencyCode(), om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getCurrency().getCurrencyCode());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), PriceVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Price_Delete_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Delete_ShouldReturn_400Response_And_ErrorCode_RES_MENU_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = priceEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Price_Delete_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPriceDetails_Amount() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        priceForm.setAmount(665.0d);

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPriceDetails_CurrencyId() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        priceForm.setCurrencyId("CAD");

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPriceDetails_ItemId() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        priceForm.setItemId(itemEntity2.getId().toString());

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Price_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenUpdatedBy_EmptyInvalidId_AndPriceDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Put_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByAbsentId_AndPriceDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Price_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_005_WhenUpdated_ByInactiveId_AndPriceDetails() throws Exception {
        String id = priceEntity2.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Price_Put_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndNoPriceDetails() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
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
    public void test_Price_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndEmptyName(String name) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        priceForm.setName(name);

        mvcResult = mockMvc.perform(put(PRICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Price_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndEmptyInvalidIsVegeterian(String isVegeterian) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "isVegeterian";
        priceForm.setIsVegeterian(isVegeterian);

        mvcResult = mockMvc.perform(put(PRICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Price_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndEmptyInvalidItemId(String itemId) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        priceForm.setItemId(itemId);

        mvcResult = mockMvc.perform(put(PRICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "99999" })
    public void test_Price_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndAbsentItemId(String itemId) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        priceForm.setItemId(itemId);

        mvcResult = mockMvc.perform(put(PRICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Put_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInactiveItemId() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String itemId = itemEntity2.getId().toString();
        String fieldName = "itemId";
        priceForm.setItemId(itemId);

        mvcResult = mockMvc.perform(put(PRICE_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(priceForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Price_Put_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndEmptyPriceDetails() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(PRICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new PriceForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Price_Put_ShouldReturn_409Response_And_ErrorCode_RES_MENU_004_WhenUpdated_ById_AndDuplicatePriceDetails() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "itemId";
        String field1Value = priceEntity1.getName();
        String field2Value = priceEntity1.getItem().getId().toString();
        priceForm.setName(field1Value);
        priceForm.setItemId(field2Value);

        mvcResult = mockMvc.perform(put(PRICE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(priceForm)))
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
    public void test_Price_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndPriceDetails() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(PRICE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndPriceDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(PRICE_URI_BY_ID, " ")
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByInvalidId_AndPriceDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_404Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ByAbsentId_AndPriceDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_409Response_And_ErrorCode_RES_MENU_002_WhenUpdated_ById_AndDuplicatePriceDetails() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "itemId";
        String field1Value = priceEntity1.getName();
        String field2Value = priceEntity1.getItem().getId().toString();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + field1Name, field1Value),
                new PatchOperationForm("replace", "/" + field2Name, field2Value));


        mvcResult = this.mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_422Response_And_ErrorCode_RES_MENU_003_WhenUpdated_ById_AndNoPriceDetails() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(PRICE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyIsVegeterian(String isVegeterian) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/isVegeterian", isVegeterian));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidIsVegeterian(String isVegeterian) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "isVegeterian";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, isVegeterian));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndEmptyItemId(String itemId) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/itemId", itemId));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidItemId(String itemId) throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "itemId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, itemId));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInactiveItemId() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String itemId = itemEntity2.getId().toString();
        String fieldName = "itemId";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, itemId));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
    public void test_Price_Patch_ShouldReturn_400Response_And_ErrorCode_RES_MENU_001_WhenRequested_ById_AndInvalidDefinitionOfPriceAttribute() throws Exception {
        String id = priceEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = MenuErrorCode.MENU_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(PRICE_URI_BY_ID, id)
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
