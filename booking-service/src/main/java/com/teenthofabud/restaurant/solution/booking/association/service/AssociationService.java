package com.teenthofabud.restaurant.solution.booking.association.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationException;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationForm;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface AssociationService {

    public Set<AssociationVo> retrieveAllByNaturalOrdering();

    public AssociationVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AssociationException;

    public List<AssociationVo> retrieveAllMatchingDetailsByExperienceId(String experienceId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AssociationException;

    public List<AssociationVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTableId,
                                                                    Optional<String> optionalAccountId) throws AssociationException;

    public String createAssociation(AssociationForm form) throws AssociationException;

    public void updateAssociation(String id, AssociationForm form) throws AssociationException;

    public void deleteAssociation(String id) throws AssociationException;

    public void applyPatchOnAssociation(String id, List<PatchOperationForm> patches) throws AssociationException;

}
