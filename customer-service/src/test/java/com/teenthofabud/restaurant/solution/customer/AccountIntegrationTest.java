package com.teenthofabud.restaurant.solution.customer;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.repository.AccountRepository;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import com.teenthofabud.restaurant.solution.customer.address.repository.AddressRepository;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.data.GenderVo;
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
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public class AccountIntegrationTest extends CustomerIntegrationBaseTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final String ACCOUNT_URI = "/account";
    private static final String ACCOUNT_URI_BY_ID = "/account/{id}";
    private static final String ACCOUNT_URI_BY_GENDER_ID = "/account/genderid/{genderId}";
    private static final String ACCOUNT_URI_FILTER = "/account/filter";

    private AccountRepository accountRepository;

    private int metadataServicePort;
    private AddressRepository addressRepository;

    @Autowired
    public void setAccountRepository(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    private GenderVo genderVo;

    private AccountForm accountForm;

    @Autowired
    public void setAddressRepository(AddressRepository addressRepository) {
        this.addressRepository = addressRepository;
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

        genderVo = new GenderVo();
        genderVo.setActive(Boolean.TRUE);
        genderVo.setName("Gender 1");
        genderVo.setId("1");

        accountForm = new AccountForm();
        accountForm.setFirstName("New First Name");
        accountForm.setLastName("New Last Name");
        accountForm.setCountryCode("91");
        accountForm.setPhoneNumber("1234567890");
        accountForm.setGenderId(genderVo.getId());

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/firstName", "patched first name"),
                new PatchOperationForm("replace", "/lastName", "patched last name"),
                new PatchOperationForm("replace", "/phoneNumber", "6666666666"));


        accountEntity1 = new AccountEntity();
        accountEntity1.setFirstName("Account 1 First Name");
        accountEntity1.setLastName("Account 1 Last Name");
        accountEntity1.setCountryCode("91");
        accountEntity1.setPhoneNumber("1122334455");
        accountEntity1.setGenderId(Long.parseLong(genderVo.getId()));
        accountEntity1.setActive(Boolean.TRUE);

        accountEntity1 = accountRepository.save(accountEntity1);

        accountVo1 = new AccountVo();
        accountVo1.setId(accountEntity1.getId().toString());
        accountVo1.setFirstName(accountEntity1.getFirstName());
        accountVo1.setLastName(accountEntity1.getLastName());
        accountVo1.setCountryCode(accountEntity1.getCountryCode());
        accountVo1.setPhoneNumber(accountEntity1.getPhoneNumber());
        accountVo1.setGender(genderVo);
        accountVo1.setGenderId(accountEntity1.getGenderId().toString());

        accountEntity2 = new AccountEntity();
        accountEntity2.setFirstName("Account 2 First Name");
        accountEntity2.setLastName("Account 2 Last Name");
        accountEntity2.setCountryCode("91");
        accountEntity2.setPhoneNumber("0987654321");
        accountEntity2.setGenderId(Long.parseLong(genderVo.getId()));
        accountEntity2.setActive(Boolean.TRUE);

        accountEntity2 = accountRepository.save(accountEntity2);

        accountVo2 = new AccountVo();
        accountVo2.setId(accountEntity2.getId().toString());
        accountVo2.setFirstName(accountEntity2.getFirstName());
        accountVo2.setLastName(accountEntity2.getLastName());
        accountVo2.setCountryCode(accountEntity2.getCountryCode());
        accountVo2.setPhoneNumber(accountEntity2.getPhoneNumber());
        accountVo2.setGender(genderVo);
        accountVo2.setGenderId(accountEntity1.getGenderId().toString());

        accountEntity3 = new AccountEntity();
        accountEntity3.setFirstName("Account 3 First Name");
        accountEntity3.setLastName("Account 3 Last Name");
        accountEntity3.setCountryCode("91");
        accountEntity3.setPhoneNumber("7766441236");
        accountEntity3.setGenderId(Long.parseLong(genderVo.getId()));
        accountEntity3.setActive(Boolean.FALSE);

        accountEntity3 = accountRepository.save(accountEntity3);

        accountVo3 = new AccountVo();
        accountVo3.setId(accountEntity3.getId().toString());
        accountVo3.setFirstName(accountEntity3.getFirstName());
        accountVo3.setLastName(accountEntity3.getLastName());
        accountVo3.setCountryCode(accountEntity3.getCountryCode());
        accountVo3.setPhoneNumber(accountEntity3.getPhoneNumber());
        accountVo3.setGender(genderVo);
        accountVo3.setGenderId(accountEntity1.getGenderId().toString());

        accountEntity4 = new AccountEntity();
        accountEntity4.setFirstName("Account 4 First Name");
        accountEntity4.setLastName("Account 4 Last Name");
        accountEntity4.setCountryCode("91");
        accountEntity4.setPhoneNumber("6299711209");
        accountEntity4.setGenderId(Long.parseLong(genderVo.getId()));
        accountEntity4.setActive(Boolean.FALSE);
        //accountEntity4.setActive(Boolean.TRUE);//here

        accountEntity4 = accountRepository.save(accountEntity4);

        accountVo4 = new AccountVo();
        accountVo4.setId(accountEntity4.getId().toString());
        accountVo4.setFirstName(accountEntity4.getFirstName());
        accountVo4.setLastName(accountEntity4.getLastName());
        accountVo4.setCountryCode(accountEntity4.getCountryCode());
        accountVo4.setPhoneNumber(accountEntity4.getPhoneNumber());
        accountVo4.setGender(genderVo);
        accountVo4.setGenderId(accountEntity1.getGenderId().toString());

        addressForm = new AddressForm();
        addressForm.setAddressLine1("New Something First");
        addressForm.setAddressLine2("New Something Next");
        addressForm.setAccountId(accountEntity4.getId().toString());
        addressForm.setCityId("133024");
        addressForm.setPincode("200012");
        addressForm.setStateId("MH");
        addressForm.setCountryId("IN");

        addressEntity1 = new AddressEntity();
        addressEntity1.setName("default");
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
        addressVo1.setName(addressEntity1.getName());
        addressVo1.setAddressLine1(addressEntity1.getAddressLine1());
        addressVo1.setAddressLine2(addressEntity1.getAddressLine2());
        addressVo1.setCountryId(addressEntity1.getCountryId());
        addressVo1.setStateId(addressEntity1.getStateId());
        addressVo1.setCityId(addressEntity1.getCityId());
        addressVo1.setPincode(addressEntity1.getPincode());
        addressVo1.setAccountId(addressEntity1.getAccount().getId().toString());
        //addressVo1.setAccount(accountVo1);

        addressEntity2 = new AddressEntity();
        addressEntity2.setName("default");
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
        addressVo2.setName(addressEntity2.getName());
        addressVo2.setAddressLine1(addressEntity2.getAddressLine1());
        addressVo2.setAddressLine2(addressEntity2.getAddressLine2());
        addressVo2.setCountryId(addressEntity2.getCountryId());
        addressVo2.setStateId(addressEntity2.getStateId());
        addressVo2.setCityId(addressEntity2.getCityId());
        addressVo2.setPincode(addressEntity2.getPincode());
        addressVo2.setAccountId(addressEntity2.getAccount().getId().toString());
        //addressVo2.setAccount(accountVo2);

        addressEntity3 = new AddressEntity();
        addressEntity3.setName("default");
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
        addressVo3.setName(addressEntity3.getName());
        addressVo3.setAddressLine1(addressEntity3.getAddressLine1());
        addressVo3.setAddressLine2(addressEntity3.getAddressLine2());
        addressVo3.setCountryId(addressEntity3.getCountryId());
        addressVo3.setStateId(addressEntity3.getStateId());
        addressVo3.setCityId(addressEntity3.getCityId());
        addressVo3.setPincode(addressEntity3.getPincode());
        addressVo3.setAccountId(addressEntity3.getAccount().getId().toString());
        //addressVo3.setAccount(accountVo3);

        addressVo4 = new AddressVo();
        addressVo4.setId(UUID.randomUUID().toString());
        addressVo4.setName(addressForm.getName());
        addressVo4.setAddressLine1(addressForm.getAddressLine1());
        addressVo4.setAddressLine2(addressForm.getAddressLine2());
        addressVo4.setCountryId(addressForm.getCountryId());
        addressVo4.setStateId(addressForm.getStateId());
        addressVo4.setCityId(addressForm.getCityId());
        addressVo4.setPincode(addressForm.getPincode());
        addressVo4.setAccountId(addressForm.getAccountId());

        accountEntity1.setAddresses(new ArrayList<>(Arrays.asList(addressEntity1)));
        accountEntity1 = accountRepository.save(accountEntity1);
        accountEntity2.setAddresses(new ArrayList<>(Arrays.asList(addressEntity2)));
        accountEntity2 = accountRepository.save(accountEntity2);
        accountEntity3.setAddresses(new ArrayList<>(Arrays.asList(addressEntity3)));
        accountEntity3 = accountRepository.save(accountEntity3);

        addressVo1.setAccount(accountVo1);
        addressVo2.setAccount(accountVo2);
        addressVo3.setAccount(accountVo3);

        accountVo1.setAddresses(Arrays.asList(addressVo1));
        accountVo2.setAddresses(Arrays.asList(addressVo2));
        accountVo3.setAddresses(Arrays.asList(addressVo3));
    }

    @AfterEach
    private void destroy() {
        addressEntity1.setAccount(null);
        addressEntity2.setAccount(null);
        addressEntity3.setAccount(null);

        addressRepository.deleteById(addressEntity1.getId());
        addressRepository.deleteById(addressEntity2.getId());
        addressRepository.deleteById(addressEntity3.getId());

        accountEntity1.setAddresses(null);
        accountEntity2.setAddresses(null);
        accountEntity3.setAddresses(null);
        accountEntity4.setAddresses(null);

        accountRepository.deleteById(accountEntity1.getId());
        accountRepository.deleteById(accountEntity2.getId());
        accountRepository.deleteById(accountEntity3.getId());
        accountRepository.deleteById(accountEntity4.getId());
    }


    @Test
    public void test_Account_Post_ShouldReturn_201Response_And_NewAccountId_WhenPosted_WithValidAccountForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(ACCOUNT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assertions.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Account_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyFirstName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "firstName";
        accountForm.setFirstName("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Account_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyLastName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "lastName";
        accountForm.setLastName("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Account_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyCountryCode() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "countryCode";
        accountForm.setCountryCode("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Account_Post_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_WithEmptyPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "phoneNumber";
        accountForm.setPhoneNumber("");

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }


    @Test
    public void test_Account_Post_ShouldReturn_409Response_And_ErrorCode_RES_CUST_004_WhenRequested_WithDuplicateAccount() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_EXISTS.getErrorCode();
        String field1Name = "phoneNumber";
        accountForm.setPhoneNumber(accountEntity1.getPhoneNumber());

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Account_Post_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenPosted_WithNoAccountForm() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(ACCOUNT_URI)
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
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAllAccounts() throws Exception {
        MvcResult mvcResult = null;
        Set<AccountVo> accountList = new TreeSet<>(Arrays.asList(accountVo1, accountVo2, accountVo3, accountVo4));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAccounts_ByGenderId() throws Exception {
        MvcResult mvcResult = null;
        List<AccountVo> accountList = Arrays.asList(accountVo1, accountVo2, accountVo3, accountVo4);

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_GENDER_ID, genderVo.getId()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAccounts_ByEmptyGenderId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "genderId";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_GENDER_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Get_ShouldReturn_404Response_And_ErrorCode_RES_CUST_001_WhenRequested_ByAbsentGenderId() throws Exception {
        MvcResult mvcResult = null;
        String genderId = "3";
        String errorCode = "TOAB-META-002";
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_GENDER_ID, genderId))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(genderId));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Account_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyFirstNameOnly(String firstName) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("firstName", firstName))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Account_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyLastNameOnly(String lastName) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("lastName", lastName))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "", " " })
    public void test_Account_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyPhoneNumberOnly(String phoneNumber) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "filters";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("phoneNumber", phoneNumber))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_EmptyAccountList_WhenRequestedBy_AbsentFirstName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("firstName", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_EmptyAccountList_WhenRequestedBy_AbsentLastName() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("lastName", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_EmptyAccountList_WhenRequestedBy_AbsentPhoneNumber() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER).queryParam("phoneNumber", "Hey"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(0, om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }


    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAccounts_WithFirstName() throws Exception {
        MvcResult mvcResult = null;
        List<AccountVo> accountList = new ArrayList<>(Arrays.asList(accountVo1, accountVo2, accountVo3, accountVo4));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("firstName", "Account"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAccounts_WithLastName() throws Exception {
        MvcResult mvcResult = null;
        List<AccountVo> accountList = new ArrayList<>(Arrays.asList(accountVo2));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("lastName", "Account 2"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAccounts_WithFirstNameAndLastName() throws Exception {
        MvcResult mvcResult = null;
        Set<AccountVo> accountList = new TreeSet<>(Arrays.asList(accountVo1));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("firstName", "Account 1")
                        .queryParam("lastName", "Account 1"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAccounts_WithPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        Set<AccountVo> accountList = new TreeSet<>(Arrays.asList(accountVo1));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("phoneNumber", "1122334455"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountListNaturallyOrdered_WhenRequested_ForAccounts_WithFirstNameAndLastNameAndPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        Set<AccountVo> accountList = new TreeSet<>(Arrays.asList(accountVo1));

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("firstName", "Account 1")
                        .queryParam("lastName", "Account 1")
                        .queryParam("phoneNumber", "1122334455"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_EmptyAccountList_WhenRequested_ForAccounts_WithAbsent_WithFirstNameAndLastName() throws Exception {
        MvcResult mvcResult = null;
        Set<AccountVo> accountList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("firstName", "Account 1")
                        .queryParam("lastName", UUID.randomUUID().toString()))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_EmptyAccountList_WhenRequested_ForAccounts_WithAbsent_WithFirstNameAndLastNameAndPhoneNumber() throws Exception {
        MvcResult mvcResult = null;
        Set<AccountVo> accountList = new TreeSet<>();

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_FILTER)
                        .queryParam("firstName", "Account 1")
                        .queryParam("lastName", UUID.randomUUID().toString())
                        .queryParam("phoneNumber", "1122334455"))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountList.size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo[].class).length);
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_AccountDetails_WhenRequested_ById() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        accountVo1.setGender(null);
        accountVo1.setAddresses(null);

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(om.writeValueAsString(accountVo1), mvcResult.getResponse().getContentAsString());
        Assertions.assertEquals(accountVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Account_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Get_ShouldReturn_400Response_And_ErrorCode_RES_CUST_002_WhenRequested_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        String id = accountEntity2.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndFirstLevel_Cascade() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.ONE.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getId());
        Assertions.assertEquals(accountVo1.getFirstName(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getFirstName());
        Assertions.assertEquals(accountVo1.getLastName(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getLastName());
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getCreatedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getModifiedBy()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getCreatedOn()));
        Assertions.assertTrue(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getModifiedOn()));
        Assertions.assertFalse(ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getActive()));
    }

    @Test
    public void test_Account_Get_ShouldReturn_200Response_And_DomainDetails_WhenRequested_ById_AndSecondLevel_Cascade() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        mvcResult = this.mockMvc.perform(get(ACCOUNT_URI_BY_ID, id)
                        .queryParam("cascadeUntilLevel", TOABCascadeLevel.TWO.getLevelCode()))
                .andDo(print())
                .andReturn();
        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(accountVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getId());
        Assertions.assertEquals(accountVo1.getFirstName(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getFirstName());
        Assertions.assertEquals(accountVo1.getLastName(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getLastName());
        Assertions.assertTrue(accountVo1.getGender().toString().compareTo(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getGender().toString()) == 0);
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getAddresses() != null);
        Assertions.assertEquals(accountVo1.getAddresses().size(), om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getAddresses().size());
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getCreatedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getModifiedBy()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getCreatedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getModifiedOn()));
        Assertions.assertTrue(!ObjectUtils.isEmpty(om.readValue(mvcResult.getResponse().getContentAsString(), AccountVo.class).getActive()));
    }

    @Test
    public void test_Account_Delete_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001__WhenDeleted_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Delete_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenDeleted_ByInvalidId() throws Exception {
        String id = " ";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Delete_ShouldReturn_400Response_And_ErrorCode_RES_CUST_005_WhenDeleted_ByInactiveId() throws Exception {
        String id = accountEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Account_Delete_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenDeleted_ByAbsentId() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndAccountDetails() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        accountForm.setFirstName("Ferran");

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Account_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenUpdatedBy_EmptyInvalidId_AndAccountDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Put_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByAbsentId_AndAccountDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Account_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_005_WhenUpdated_ByInactiveId_AndAccountDetails() throws Exception {
        String id = accountEntity4.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Account_Put_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndNoAccountDetails() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
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
    public void test_Account_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidFirstName() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "firstName";
        accountForm.setFirstName("");

        mvcResult = mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Put_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidLastName() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "lastName";
        accountForm.setLastName("");

        mvcResult = mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Account_Put_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndEmptyAccountDetails() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new AccountForm())))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Account_Put_ShouldReturn_409Response_And_ErrorCode_RES_CUST_004_WhenUpdated_ById_AndDuplicateAccountDetails() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_EXISTS.getErrorCode();
        String field1Name = "phoneNumber";
        accountForm.setPhoneNumber(accountEntity2.getPhoneNumber());

        mvcResult = mockMvc.perform(put(ACCOUNT_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(accountForm)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Account_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndAccountDetails() throws Exception {
        String id = accountEntity4.getId().toString();
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Account_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndAccountDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, " ")
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
    public void test_Account_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByInvalidId_AndAccountDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Account_Patch_ShouldReturn_404Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ByAbsentId_AndAccountDetails() throws Exception {
        String id = "5";
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Account_Patch_ShouldReturn_409Response_And_ErrorCode_RES_CUST_002_WhenUpdated_ById_AndDuplicateAccountDetails() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_EXISTS.getErrorCode();
        String fieldName = "phoneNumber";
        String fieldValue = accountEntity3.getPhoneNumber();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/phoneNumber", fieldValue));


        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Account_Patch_ShouldReturn_422Response_And_ErrorCode_RES_CUST_003_WhenUpdated_ById_AndNoAccountDetails() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assertions.assertNotNull(mvcResult);
        Assertions.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assertions.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assertions.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Account_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Account_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
    public void test_Account_Patch_ShouldReturn_400Response_And_ErrorCode_RES_CUST_001_WhenRequested_ById_AndInvalidDefinitionOfAccountAttribute() throws Exception {
        String id = accountEntity1.getId().toString();
        MvcResult mvcResult = null;
        String errorCode = CustomerErrorCode.CUST_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(ACCOUNT_URI_BY_ID, id)
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
