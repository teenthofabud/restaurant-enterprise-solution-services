package com.teenthofabud.restaurant.solution.print.template.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateException;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateForm;
import com.teenthofabud.restaurant.solution.print.template.data.TemplateVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface TemplateService {

    public Set<TemplateVo> retrieveAllByNaturalOrdering();

    public TemplateVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TemplateException;

    public List<TemplateVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                 Optional<String> optionalDescription,
                                                                 Optional<String> optionalTemplateTypeId) throws TemplateException;

    public String createTemplate(TemplateForm form) throws TemplateException;

    public void updateTemplate(String id, TemplateForm form) throws TemplateException;

    public void deleteTemplate(String id) throws TemplateException;

    public void applyPatchOnTemplate(String id, List<PatchOperationForm> patches) throws TemplateException;

}
