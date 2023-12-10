package com.teenthofabud.restaurant.solution.settings.internationalization.service.impl;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.restaurant.solution.settings.discount.data.DiscountDocument;
import com.teenthofabud.restaurant.solution.settings.error.SettingsErrorCode;
import com.teenthofabud.restaurant.solution.settings.internationalization.converter.LanguageForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.*;
import com.teenthofabud.restaurant.solution.settings.internationalization.repository.LanguageRepository;
import com.teenthofabud.restaurant.solution.settings.internationalization.service.InternationalizationService;
import com.teenthofabud.restaurant.solution.settings.internationalization.validator.LanguageFormValidator;
import com.teenthofabud.restaurant.solution.settings.utils.SettingsServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.util.*;

@Component
@Slf4j
public class LanguageService implements InternationalizationService {
    private SettingsServiceHelper settingsServiceHelper;

    @Autowired
    private LanguageForm2DocumentConverter formToDocumentConverter;

    @Autowired
    private LanguageRepository repository;

    @Autowired
    private LanguageFormValidator validator;


    @Autowired
    public void setSettingsServiceHelper(SettingsServiceHelper settingsServiceHelper) { this.settingsServiceHelper = settingsServiceHelper; }

    @Override
    public Set<LanguageVo> retrieveAllByNaturalOrdering() {
        log.info("Retrieve all Languages by their natural ordering");
        List<LanguageDocument> languageDocumentList = repository.findAll();
        List<LanguageVo> languageVoList = settingsServiceHelper.languageDocument2DetailedVo(languageDocumentList);
        Set<LanguageVo> languageVoSet = new TreeSet<>();
        languageVoSet.addAll(languageVoList);
        return languageVoSet;
    }

    @Override
    public String createLanguage(LanguageForm form) throws LanguageException {

        log.info("Creating new Language");
        LanguageDocument expectedDocument = formToDocumentConverter.convert(form);
        log.debug("validating provided attributes of language form");
        Errors errors = new DirectFieldBindingResult(form, form.getClass().getName());
        validator.validate(form, errors);
        if(repository.existsByName(expectedDocument.getName())){
            log.debug(LanguageTemplate.MSG_TEMPLATE_LANGUAGE_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());
            throw new LanguageException(SettingsErrorCode.SETTINGS_EXISTS, new Object[] {"name", form.getName()});
        }

        log.debug(LanguageTemplate.MSG_TEMPLATE_LANGUAGE_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());
        log.debug("Saving {}", expectedDocument);
        LanguageDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null){
            log.debug("Unable to create {}", expectedDocument);
            throw new LanguageException(SettingsErrorCode.SETTINGS_ACTION_FAILURE,
                    new Object[] { "Creation", "Unable to persist language"});
        }
        log.info(" Created language by id: {} ", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public LanguageVo retrieveLanguageById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws LanguageException {
        log.info("Requesting language by id: {}", id);
        Optional<LanguageDocument> languageDocument = repository.findById(id);
        if(!languageDocument.isPresent()){
            log.debug("No language found with id: {}", id);
            throw new LanguageException(SettingsErrorCode.SETTINGS_NOT_FOUND, new Object[]{"id", String.valueOf(id) });
        }
        log.info("Found language by id: {}", id);
        LanguageDocument entity = languageDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        LanguageVo vo = settingsServiceHelper.languageDocument2DetailedVo(entity);
        log.debug("LanguageVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<LanguageVo> retrieveAllMatchingDetailsByCriteria(String name, String code) {

        List<LanguageVo> matchedLanguageList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        LanguageDocument entity = new LanguageDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(code))) {
            log.debug("code {} is valid", code);
            providedFilters.put("code", code);
            entity.setCode(code);
            matcherCriteria = matcherCriteria.withMatcher("code", match -> match.contains());
        }

        if(providedFilters.isEmpty()){
            log.debug("Search parameters are not valid");
        }else{
            log.debug("Search parameters are valid {}", providedFilters);
        }

        Example<LanguageDocument> discountDocumentExample = Example.of(entity, matcherCriteria);
        List<LanguageDocument> engagementDocumentList = repository.findAll(discountDocumentExample);
        matchedLanguageList = settingsServiceHelper.languageDocument2DetailedVo(engagementDocumentList);
        log.info("Found {} DiscountVo matching with provided parameters : {}", matchedLanguageList.size(), providedFilters);
        log.info("No DiscountVo available matching with provided parameters : {}", matchedLanguageList.size(), providedFilters);
        return matchedLanguageList;
    }


}
