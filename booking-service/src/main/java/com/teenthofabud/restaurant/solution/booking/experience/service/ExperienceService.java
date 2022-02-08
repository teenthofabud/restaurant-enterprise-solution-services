package com.teenthofabud.restaurant.solution.booking.experience.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceException;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceForm;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface ExperienceService {

    public Set<ExperienceVo> retrieveAllByNaturalOrdering();

    public ExperienceVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ExperienceException;

    public List<ExperienceVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                                    Optional<String> optionalDescription) throws ExperienceException;

    public String createExperience(ExperienceForm form) throws ExperienceException;

    public void updateExperience(String id, ExperienceForm form) throws ExperienceException;

    public void deleteExperience(String id) throws ExperienceException;

    public void applyPatchOnExperience(String id, List<PatchOperationForm> patches) throws ExperienceException;

}
