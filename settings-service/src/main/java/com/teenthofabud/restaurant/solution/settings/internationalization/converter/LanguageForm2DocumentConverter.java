package com.teenthofabud.restaurant.solution.settings.internationalization.converter;

import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageDocument;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class LanguageForm2DocumentConverter implements Converter<LanguageForm, LanguageDocument> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.settings.language.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public LanguageDocument convert(LanguageForm form) {

        LanguageDocument entity = new LanguageDocument();
        if(!fieldsToEscape.contains("name")) {
            entity.setName(form.getName());
        }
        if(!fieldsToEscape.contains("code")) {
            entity.setCode(form.getCode());
        }
        if(!fieldsToEscape.contains("idiom")) {
            entity.setIdiom(form.getIdiom());
        }
        if(!fieldsToEscape.contains("canDelete")) {
            entity.setCanDelete(form.isDeletable());
        }
        log.debug("Converted {} to {} ", form, entity);

        return entity;
    }
}
