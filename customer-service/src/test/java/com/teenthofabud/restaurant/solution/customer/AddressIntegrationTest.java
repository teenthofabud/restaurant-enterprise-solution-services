package com.teenthofabud.restaurant.solution.customer;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.repository.AccountRepository;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import com.teenthofabud.restaurant.solution.customer.address.repository.AddressRepository;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
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

import javax.persistence.EntityManager;
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
public class AddressIntegrationTest extends CustomerIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ADDRESS_URI = "/address";
    private static final String ADDRESS_URI_BY_ID = "/address/{id}";
    private static final String ADDRESS_URI_BY_ACCOUNT_ID = "/address/accountid/{accountId}";
    private static final String ADDRESS_URI_FILTER = "/address/filter";

    private AddressRepository addressRepository;
    private AccountRepository accountRepository;

    private EntityManager em;

    @Autowired
    public void setEm(EntityManager em) {
        this.em = em;
    }

    private int metadataServicePort;

    @Value("${customer.metadata.service.port}")
    public void setMetadataServicePort(int metadataServicePort) {
        this.metadataServicePort = metadataServicePort;
    }

    @Autowired
    public void setAddressRepository(AddressRepository addressRepository) {
        this.addressRepository = addressRepository;
    }

    @Autowired
    public void setAccountRepository(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    private AccountVo accountVo1;
    private AccountVo accountVo2;
    private AccountVo accountVo3;
    private AccountVo accountVo4;
    private AccountEntity accountEntity1;
    private AccountEntity accountEntity2;
    private AccountEntity accountEntity3;
    private AccountEntity accountEntity4;

    private AddressForm addressForm;
    private AddressVo addressVo1;
    private AddressVo addressVo2;
    private AddressVo addressVo3;
    private AddressVo addressVo4;
    private AddressEntity addressEntity1;
    private AddressEntity addressEntity2;
    private AddressEntity addressEntity3;

    private List<PatchOperationForm> patches;

    @BeforeEach
    private void init() {

        accountEntity1 = new AccountEntity();
        accountEntity1.setFirstName("Account 1 First Name");
        accountEntity1.setLastName("Account 1 Last Name");
        accountEntity1.setCountryCode("91");
        accountEntity1.setPhoneNumber("1122334455");
        accountEntity1.setActive(Boolean.TRUE);

        accountEntity1 = accountRepository.save(accountEntity1);

        accountVo1 = new AccountVo();
        accountVo1.setId(accountEntity1.getId().toString());
        accountVo1.setFirstName(accountEntity1.getFirstName());
        accountVo1.setLastName(accountEntity1.getLastName());
        accountVo1.setCountryCode(accountEntity1.getCountryCode());
        accountVo1.setPhoneNumber(accountEntity1.getPhoneNumber());

        accountEntity2 = new AccountEntity();
        accountEntity2.setFirstName("Account 2 First Name");
        accountEntity2.setLastName("Account 2 Last Name");
        accountEntity2.setCountryCode("91");
        accountEntity2.setPhoneNumber("0987654321");
        accountEntity2.setActive(Boolean.TRUE);

        accountEntity2 = accountRepository.save(accountEntity2);

        accountVo2 = new AccountVo();
        accountVo2.setId(accountEntity2.getId().toString());
        accountVo2.setFirstName(accountEntity2.getFirstName());
        accountVo2.setLastName(accountEntity2.getLastName());
        accountVo2.setCountryCode(accountEntity2.getCountryCode());
        accountVo2.setPhoneNumber(accountEntity2.getPhoneNumber());

        accountEntity3 = new AccountEntity();
        accountEntity3.setFirstName("Account 3 First Name");
        accountEntity3.setLastName("Account 3 Last Name");
        accountEntity3.setCountryCode("91");
        accountEntity3.setPhoneNumber("7766441236");
        accountEntity3.setActive(Boolean.FALSE);

        accountEntity3 = accountRepository.save(accountEntity3);

        accountVo3 = new AccountVo();
        accountVo3.setId(accountEntity3.getId().toString());
        accountVo3.setFirstName(accountEntity3.getFirstName());
        accountVo3.setLastName(accountEntity3.getLastName());
        accountVo3.setCountryCode(accountEntity3.getCountryCode());
        accountVo3.setPhoneNumber(accountEntity3.getPhoneNumber());

        accountEntity4 = new AccountEntity();
        accountEntity4.setFirstName("Account 4 First Name");
        accountEntity4.setLastName("Account 4 Last Name");
        accountEntity4.setCountryCode("91");
        accountEntity4.setPhoneNumber("6299711209");
        accountEntity4.setActive(Boolean.TRUE);//here

        accountEntity4 = accountRepository.save(accountEntity4);

        accountVo4 = new AccountVo();
        accountVo4.setId(accountEntity4.getId().toString());
        accountVo4.setFirstName(accountEntity4.getFirstName());
        accountVo4.setLastName(accountEntity4.getLastName());
        accountVo4.setCountryCode(accountEntity4.getCountryCode());
        accountVo4.setPhoneNumber(accountEntity4.getPhoneNumber());

        addressForm = new AddressForm();
        addressForm.setAddressLine1("New Something First");
        addressForm.setAddressLine2("New Something Next");
        addressForm.setAccountId(accountEntity4.getId().toString());
        addressForm.setCityId("133024");
        addressForm.setPincode("200012");
        addressForm.setStateId("MH");
        addressForm.setCountryId("IN");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/addressLine1", "patched first name"),
                new PatchOperationForm("replace", "/addressLine2", "patched last name"),
                new PatchOperationForm("replace", "/name", "patched name"));

        addressEntity1 = new AddressEntity();
        addressEntity1.setAddressLine1("Address 1 First Name");
        addressEntity1.setAddressLine2("Address 1 Last Name");
        addressEntity1.setCountryId("IN");
        addressEntity1.setStateId("MH");
        addressEntity1.setCityId("133024");
        addressEntity1.setPincode("200088");
        addressEntity1.setActive(Boolean.TRUE);
        addressEntity1.setAccount(accountEntity1);

        addressEntity1 = addressRepository.save(addressEntity1);

        addressVo1 = new AddressVo();
        addressVo1.setId(addressEntity1.getId().toString());
        addressVo1.setAddressLine1(addressEntity1.getAddressLine1());
        addressVo1.setAddressLine2(addressEntity1.getAddressLine2());
        addressVo1.setCountryId(addressEntity1.getCountryId());
        addressVo1.setStateId(addressEntity1.getStateId());
        addressVo1.setCityId(addressEntity1.getCityId());
        addressVo1.setPincode(addressEntity1.getPincode());
        addressVo1.setAccountId(addressEntity1.getAccount().getId().toString());
        addressVo1.setAccount(accountVo1);

        addressEntity2 = new AddressEntity();
        addressEntity2.setAddressLine1("Address 2 First Name");
        addressEntity2.setAddressLine2("Address 2 Last Name");
        addressEntity2.setCountryId("IN");
        addressEntity2.setStateId("MH");
        addressEntity2.setCityId("133024");
        addressEntity2.setPincode("200111");
        addressEntity2.setActive(Boolean.TRUE);
        addressEntity2.setAccount(accountEntity2);

        addressEntity2 = addressRepository.save(addressEntity2);

        addressVo2 = new AddressVo();
        addressVo2.setId(addressEntity2.getId().toString());
        addressVo2.setAddressLine1(addressEntity2.getAddressLine1());
        addressVo2.setAddressLine2(addressEntity2.getAddressLine2());
        addressVo2.setCountryId(addressEntity2.getCountryId());
        addressVo2.setStateId(addressEntity2.getStateId());
        addressVo2.setCityId(addressEntity2.getCityId());
        addressVo2.setPincode(addressEntity2.getPincode());
        addressVo2.setAccountId(addressEntity2.getAccount().getId().toString());
        addressVo2.setAccount(accountVo2);

        addressEntity3 = new AddressEntity();
        addressEntity3.setAddressLine1("Address 3 First Name");
        addressEntity3.setAddressLine2("Address 3 Last Name");
        addressEntity3.setCountryId("IN");
        addressEntity3.setStateId("MH");
        addressEntity3.setCityId("133024");
        addressEntity3.setPincode("200009");
        addressEntity3.setActive(Boolean.FALSE);
        addressEntity3.setAccount(accountEntity3);

        addressEntity3 = addressRepository.save(addressEntity3);

        addressVo3 = new AddressVo();
        addressVo3.setId(addressEntity3.getId().toString());
        addressVo3.setAddressLine1(addressEntity3.getAddressLine1());
        addressVo3.setAddressLine2(addressEntity3.getAddressLine2());
        addressVo3.setCountryId(addressEntity3.getCountryId());
        addressVo3.setStateId(addressEntity3.getStateId());
        addressVo3.setCityId(addressEntity3.getCityId());
        addressVo3.setPincode(addressEntity3.getPincode());
        addressVo3.setAccountId(addressEntity3.getAccount().getId().toString());
        addressVo3.setAccount(accountVo3);

        addressVo4 = new AddressVo();
        addressVo4.setId(UUID.randomUUID().toString());
        addressVo4.setAddressLine1(addressForm.getAddressLine1());
        addressVo4.setAddressLine2(addressForm.getAddressLine2());
        addressVo4.setCountryId(addressForm.getCountryId());
        addressVo4.setStateId(addressForm.getStateId());
        addressVo4.setCityId(addressForm.getCityId());
        addressVo4.setPincode(addressForm.getPincode());
        addressVo4.setAccountId(addressForm.getAccountId());

        /*Optional<AddressEntity> optAE1 = addressRepository.findById(addressEntity1.getId());
        Optional<AddressEntity> optAE2 = addressRepository.findById(addressEntity2.getId());
        Optional<AddressEntity> optAE3 = addressRepository.findById(addressEntity3.getId());

        log.info(optAE1.get().toString());
        log.info(optAE2.get().toString());
        log.info(optAE3.get().toString());

        Query q = em.createNativeQuery("select * from customer_address");
        List<Object[]> r = q.getResultList();
        log.info("result set size " + r.size());
        for(int i = 0 ; i < r.size() ; i++) {
            Object[] o = r.get(i);
            log.info(" index " + i + " has " + o.length + " columns");
            List<String> f = new ArrayList<>(o.length);
            for(Object j : o) {
                f.add(j != null ? j.toString() : "");
            }
            log.info(String.join(",", f));
        }

        AddressEntity ae1 = em.find(AddressEntity.class, accountEntity1.getId());
        AddressEntity ae2 = em.find(AddressEntity.class, accountEntity2.getId());
        AddressEntity ae3 = em.find(AddressEntity.class, accountEntity3.getId());

        log.info(ae1.toString());
        log.info(ae2.toString());
        log.info(ae3.toString());*/
    }

    @AfterEach
    private void destroy() {
        addressEntity1.setAccount(null);
        addressEntity2.setAccount(null);
        addressEntity3.setAccount(null);
        addressRepository.deleteById(addressEntity1.getId());
        addressRepository.deleteById(addressEntity2.getId());
        addressRepository.deleteById(addressEntity3.getId());
        accountRepository.deleteById(accountEntity1.getId());
        accountRepository.deleteById(accountEntity2.getId());
        accountRepository.deleteById(accountEntity3.getId());
        accountRepository.deleteById(accountEntity4.getId());
    }

    @Test
    public void test_Address_Post_ShouldReturn_201Response_And_NewAddressId_WhenPosted_WithValidAddressForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ADDRESS_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Address_Post_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_WithEmptyAddressLine1() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "addressLine1";
        addressForm.setAddressLine1("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Address_Post_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_WithEmptyCityId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cityId";
        addressForm.setCityId("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Address_Post_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_WithEmptyPincode() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "pincode";
        addressForm.setPincode("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Address_Post_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_WithEmptyStateId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "stateId";
        addressForm.setStateId("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Address_Post_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_WithEmptyCountryId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "countryId";
        addressForm.setCountryId("");

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Address_Post_ShouldReturn_409Response_And_ErrorCode_PHARM_LEARN_CUST_004_WhenRequested_WithDuplicateAddress() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_EXISTS.getErrorCode();
        String field1Name = "name";
        String field2Name = "accountId";
        addressForm.setAccountId(addressEntity2.getAccount().getId().toString());

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field2Name));
    }

    @Test
    public void test_Address_Post_ShouldReturn_422Response_And_ErrorCode_PHARM_LEARN_CUST_003_WhenPosted_WithNoAddressForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ADDRESS_URI)
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
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAllAddresses() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = new ArrayList<>(Arrays.asList(addressVo1, addressVo2, addressVo3, addressVo4));

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_ByAccountId() throws Exception {
        MvcResult mvcResult = null;

        List<AddressVo> addressList = Arrays.asList(addressVo1);
        addressVo1.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ACCOUNT_ID, accountEntity1.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_ByEmptyAccountId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "accountId";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ACCOUNT_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Address_Get_ShouldReturn_404Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ByAbsentAccountId() throws Exception {
        MvcResult mvcResult = null;
        String albumId = "kk";
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "accountId";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ACCOUNT_ID, albumId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(albumId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Address_Get_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequestedBy_EmptyAddressLine1Only(String addressLine1) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("addressLine1", addressLine1))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Address_Get_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequestedBy_EmptyPincodeOnly(String pincode) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("pincode", pincode))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Address_Get_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequestedBy_EmptyCityIdOnly(String cityId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("cityId", cityId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Address_Get_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequestedBy_EmptyStateIdOnly(String stateId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("stateId", stateId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Address_Get_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequestedBy_EmptyCountryIdOnly(String countryId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("countryId", countryId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_EmptyAddressList_WhenRequestedBy_AbsentAddressLine1Name() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("addressLine1", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_EmptyAddressList_WhenRequestedBy_AbsentPincode() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("pincode", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_EmptyAddressList_WhenRequestedBy_AbsentCityId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("cityId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_EmptyAddressList_WhenRequestedBy_AbsentStateId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("stateId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_EmptyAddressList_WhenRequestedBy_AbsentCountryId() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER).queryParam("countryId", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }


    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithAddressLine1() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = new ArrayList<>(Arrays.asList(addressVo1));
        addressVo1.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("addressLine1", "Address 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithAddressLine2() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = new ArrayList<>(Arrays.asList(addressVo1));
        addressVo1.setAccount(null);
        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("addressLine2", "Address 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithPincode() throws Exception {
        MvcResult mvcResult = null;
        addressVo2.setAccount(null);
        List<AddressVo> addressList = new ArrayList<>(Arrays.asList(addressVo2));

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("pincode", "200111"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithCityId() throws Exception {
        MvcResult mvcResult = null;
        addressVo1.setAccount(null);
        addressVo2.setAccount(null);
        addressVo3.setAccount(null);
        List<AddressVo> addressList = new ArrayList<>(Arrays.asList(addressVo1, addressVo2, addressVo3));
        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", "133024"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithStateId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = new ArrayList<>(Arrays.asList(addressVo1, addressVo2, addressVo3));
        addressVo1.setAccount(null);
        addressVo2.setAccount(null);
        addressVo3.setAccount(null);


        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("stateId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = new ArrayList<>(Arrays.asList(addressVo1, addressVo2, addressVo3));
        addressVo1.setAccount(null);
        addressVo2.setAccount(null);
        addressVo3.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("countryId", "IN"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithStateIdAndCityId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = Arrays.asList(addressVo1, addressVo2, addressVo3);
        addressVo1.setAccount(null);
        addressVo2.setAccount(null);
        addressVo3.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", "133024")
                        .queryParam("stateId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithCountryIdAndStateId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = Arrays.asList(addressVo1, addressVo2, addressVo3);
        addressVo1.setAccount(null);
        addressVo2.setAccount(null);
        addressVo3.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("countryId", "IN")
                        .queryParam("stateId", "MH"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithCountryIdAndCityId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = Arrays.asList(addressVo1, addressVo2, addressVo3);
        addressVo1.setAccount(null);
        addressVo2.setAccount(null);
        addressVo3.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("countryId", "IN")
                        .queryParam("cityId", "133024"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithCityIdAndPincode() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = Arrays.asList(addressVo2);
        addressVo2.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", "133024")
                        .queryParam("pincode", "200111"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressListNaturallyOrdered_WhenRequested_ForAddresses_WithAddressLine1AndPincodeAndCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = Arrays.asList(addressVo2);
        addressVo2.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("addressLine1", "Address 2")
                        .queryParam("pincode", "200111")
                        .queryParam("countryId", "IN"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
        Assertions.assertEquals(om.writeValueAsString(addressList), mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_EmptyAddressList_WhenRequested_ForAddresses_WithAbsent_AddressLine1AndCityIdAndCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("addressLine1", "Address 1")
                        .queryParam("cityId", UUID.randomUUID().toString())
                        .queryParam("countryId", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_EmptyAddressList_WhenRequested_ForAddresses_WithAbsent_CityIdAndStateIdAndCountryId() throws Exception {
        MvcResult mvcResult = null;
        List<AddressVo> addressList = new ArrayList<>();

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_FILTER)
                        .queryParam("cityId", UUID.randomUUID().toString())
                        .queryParam("stateId", UUID.randomUUID().toString())
                        .queryParam("countryId", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo[].class).length);
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_AddressDetails_WhenRequested_ById() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        addressVo1.setAccount(null);

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(addressVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(addressVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Address_Get_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Address_Get_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Address_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = addressEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ADDRESS_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(addressVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getId());
        Assertions.assertEquals(addressVo1.getAddressLine1(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getAddressLine1());
        Assertions.assertEquals(addressVo1.getAddressLine2(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getAddressLine2());
        Assertions.assertEquals(addressVo1.getPincode(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getPincode());
        Assertions.assertEquals(addressVo1.getCityId(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getCityId());
        Assertions.assertEquals(addressVo1.getStateId(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getStateId());
        Assertions.assertEquals(addressVo1.getCountryId(), om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getCountryId());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AddressVo.class).getActive()));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Address_Delete_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001__WhenDeleted_ByEmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    /*@Test
    public void test_Address_Delete_ShouldReturn_422Response_And_ErrorCode_PHARM_LEARN_CUST_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @Test
    public void test_Address_Delete_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = addressEntity3.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Address_Delete_ShouldReturn_404Response_And_ErrorCode_PHARM_LEARN_CUST_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Address_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndAddressDetails() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        addressForm.setAddressLine1("Ferran");

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Address_Put_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenUpdatedBy_EmptyInvalidId_AndAddressDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Address_Put_ShouldReturn_404Response_And_ErrorCode_PHARM_LEARN_CUST_002_WhenUpdated_ByAbsentId_AndAddressDetails() throws Exception {
        String id = "55";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Address_Put_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_005_WhenUpdated_ByInactiveId_AndAddressDetails() throws Exception {
        String id = addressEntity3.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Address_Put_ShouldReturn_422Response_And_ErrorCode_PHARM_LEARN_CUST_003_WhenUpdated_ById_AndNoAddressDetails() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    /*@Test
    public void test_Address_Put_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ById_AndInvalidAddressLine1() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "addressLine1";
        addressForm.setPincode("");

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }*/

    @ParameterizedTest
    @ValueSource(strings = { " ", "" })
    public void test_Address_Put_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ById_AndEmptyAddressLine1(String addressLine1) throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "addressLine1";
        addressForm.setAddressLine1(addressLine1);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Address_Put_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ById_AndEmptyInvalidCityId(String cityId) throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "cityId";
        addressForm.setCityId(cityId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Address_Put_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ById_AndEmptyInvalidStateId(String stateId) throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "stateId";
        addressForm.setStateId(stateId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "", "r" })
    public void test_Address_Put_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ById_AndEmptyInvalidCountryId(String countryId) throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "countryId";
        addressForm.setCountryId(countryId);

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Address_Put_ShouldReturn_422Response_And_ErrorCode_PHARM_LEARN_CUST_003_WhenUpdated_ById_AndEmptyAddressDetails() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new AddressForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Address_Put_ShouldReturn_409Response_And_ErrorCode_PHARM_LEARN_CUST_004_WhenUpdated_ById_AndDuplicateAddressDetails() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_EXISTS.getErrorCode();
        String field1Name = "phoneNumber";
        addressForm.setAccountId(addressEntity2.getAccount().getId().toString());

        mvcResult = mockMvc.perform(put(ADDRESS_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(addressForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Address_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndAddressDetails() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Address_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndAddressDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, " ")
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
    public void test_Address_Patch_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_002_WhenUpdated_ByInvalidId_AndAddressDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Address_Patch_ShouldReturn_404Response_And_ErrorCode_PHARM_LEARN_CUST_002_WhenUpdated_ByAbsentId_AndAddressDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Address_Patch_ShouldReturn_409Response_And_ErrorCode_PHARM_LEARN_CUST_002_WhenUpdated_ById_AndDuplicateAddressDetails() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_EXISTS.getErrorCode();
        String fieldName = "accountId";
        String fieldValue = addressEntity2.getAccount().getId().toString();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));


        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_Address_Patch_ShouldReturn_422Response_And_ErrorCode_PHARM_LEARN_CUST_003_WhenUpdated_ById_AndNoAddressDetails() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ADDRESS_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Address_Patch_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Address_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public void test_Address_Patch_ShouldReturn_400Response_And_ErrorCode_PHARM_LEARN_CUST_001_WhenRequested_ById_AndInvalidDefinitionOfAddressAttribute() throws Exception {
        String id = addressEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ADDRESS_URI_BY_ID, id)
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
    public String getSimulationBaseLocation() {
        return "simulation/metadata-service";
    }

    @Override
    public Integer getServicePort() {
        return this.metadataServicePort;
    }

    @Override
    public String[] getSimulationFilePaths() {
        return new String[] { String.join("/", getSimulationBaseLocation(), "simulation-v3.json") };
    }
}
