package com.teenthofabud.restaurant.solution.customer.account.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface AccountService {

    public Set<AccountVo> retrieveAllByNaturalOrdering();

    @Deprecated
    public AccountVo retrieveDetailsById(String id) throws AccountException;

    public AccountVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AccountException;

    public List<AccountVo> retrieveAllMatchingDetailsByGenderId(String genderId) throws AccountException;


    public List<AccountVo> retrieveAllMatchingDetailsByFirstNameLastNameDateOfBirth(Optional<String> optionalFirstName,
                                                                                      Optional<String> optionalLastName,
                                                                           Optional<String> optionalDateOfBirth) throws AccountException;

    public String createAccount(AccountForm form) throws AccountException;

    public void updateAccount(String id, AccountForm form) throws AccountException;

    public void deleteAccount(String id) throws AccountException;

    public void applyPatchOnAccount(String id, List<PatchOperationForm> patches) throws AccountException;

}
