package com.teenthofabud.restaurant.solution.settings.internationalization.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageException;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageForm;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageVo;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface InternationalizationService {

    Set<LanguageVo> retrieveAllByNaturalOrdering();

    String createLanguage(LanguageForm form) throws LanguageException;

    LanguageVo retrieveLanguageById(String id, Optional<TOABCascadeLevel> empty) throws LanguageException;

    List<LanguageVo> retrieveAllMatchingDetailsByCriteria(String name, String code);
}
