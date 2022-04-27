package com.teenthofabud.restaurant.solution.reservation.category.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class CategoryForm2DocumentMapper implements DualChannelMapper<CategoryDocument, CategoryForm> {

    private List<String> fieldsToEscape;

    @Value("#{'${res.booking.category.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    public Optional<CategoryDocument> compareAndMap(CategoryDocument actualDocument, CategoryForm form) {
        CategoryDocument expectedDocument = new CategoryDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying CategoryDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying CategoryDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying CategoryDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy
        if(!fieldsToEscape.contains("name") && StringUtils.hasText(StringUtils.trimWhitespace(form.getName()))
                && form.getName().compareTo(actualDocument.getName()) != 0) {
            expectedDocument.setName(form.getName());
            changeSW = true;
            log.debug("CategoryForm.name: {} is different as CategoryDocument.name: {}", form.getName(), actualDocument.getName());
        } else {
            expectedDocument.setName(actualDocument.getName());
            log.debug("CategoryForm.name: is unchanged");
        }
        if(!fieldsToEscape.contains("description") && StringUtils.hasText(StringUtils.trimWhitespace(form.getDescription()))
                && form.getDescription().compareTo(actualDocument.getDescription()) != 0) {
            expectedDocument.setDescription(form.getDescription());
            changeSW = true;
            log.debug("CategoryForm.description: {} is different as CategoryDocument.description: {}", form.getDescription(), actualDocument.getDescription());
        } else {
            expectedDocument.setDescription(actualDocument.getDescription());
            log.debug("CategoryForm.description: is unchanged");
        }
        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
