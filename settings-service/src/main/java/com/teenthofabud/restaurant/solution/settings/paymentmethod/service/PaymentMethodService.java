package com.teenthofabud.restaurant.solution.settings.paymentmethod.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodException;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodForm;
import com.teenthofabud.restaurant.solution.settings.paymentmethod.data.PaymentMethodVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface PaymentMethodService {

    public Set<PaymentMethodVo> retrieveAllByNaturalOrdering();

    public PaymentMethodVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws PaymentMethodException;

    public List<PaymentMethodVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                                    Optional<String> optionalDescription) throws PaymentMethodException;

    public String createPaymentMethod(PaymentMethodForm form) throws PaymentMethodException;

    public void updatePaymentMethod(String id, PaymentMethodForm form) throws PaymentMethodException;

    public void deletePaymentMethod(String id) throws PaymentMethodException;

    public void applyPatchOnPaymentMethod(String id, List<PatchOperationForm> patches) throws PaymentMethodException;

}
