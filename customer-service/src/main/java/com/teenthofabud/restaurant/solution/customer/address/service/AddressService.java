package com.teenthofabud.restaurant.solution.customer.address.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressException;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface AddressService {

    public Set<AddressVo> retrieveAllByNaturalOrdering();

    public AddressVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AddressException;

    public List<AddressVo> retrieveAllMatchingDetailsByAccountId(String accountId) throws AddressException;

    public List<AddressVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                                                      Optional<String> optionalPincode,
                                                                                                      Optional<String> optionalCityId,
                                                                                                      Optional<String> optionalStateId,
                                                                                                      Optional<String> optionalCountryId) throws AddressException;

    public String createAddress(AddressForm form) throws AddressException;

    public void updateAddress(String id, AddressForm form) throws AddressException;

    public void deleteAddress(String id) throws AddressException;

    public void applyPatchOnAddress(String id, List<PatchOperationForm> patches) throws AddressException;

}
