package com.teenthofabud.restaurant.solution.settings.deliverypartner.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerException;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerForm;
import com.teenthofabud.restaurant.solution.settings.deliverypartner.data.DeliveryPartnerVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface DeliveryPartnerService {

    public Set<DeliveryPartnerVo> retrieveAllByNaturalOrdering();

    public DeliveryPartnerVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws DeliveryPartnerException;

    public List<DeliveryPartnerVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                        Optional<String> optionalDescription) throws DeliveryPartnerException;

    public String createDeliveryPartner(DeliveryPartnerForm form) throws DeliveryPartnerException;

    public void updateDeliveryPartner(String id, DeliveryPartnerForm form) throws DeliveryPartnerException;

    public void deleteDeliveryPartner(String id) throws DeliveryPartnerException;

    public void applyPatchOnDeliveryPartner(String id, List<PatchOperationForm> patches) throws DeliveryPartnerException;

}
