package com.teenthofabud.restaurant.solution.customer.account.converter;

import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Component
@Slf4j
public class AccountForm2EntityConverter implements Converter<AccountForm, AccountEntity> {

    private String dobFormat;
    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.customer.dob.format}")
    public void setDobFormat(String dobFormat) {
        this.dobFormat = dobFormat;
    }

    @Override
    public AccountEntity convert(AccountForm form) {
        AccountEntity entity = new AccountEntity();
        entity.setFirstName(form.getFirstName());
        entity.setLastName(form.getLastName());
        if(!fieldsToEscape.contains("dateOfBirth")) {
            DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dobFormat);
            LocalDate dateOfBirth = LocalDate.parse(form.getDateOfBirth(), dtf);
            entity.setDateOfBirth(dateOfBirth);
        }
        if(!fieldsToEscape.contains("genderId")) {
            entity.setGenderId(Long.parseLong(form.getGenderId()));
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
