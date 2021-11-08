package com.teenthofabud.restaurant.solution.customer.account.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountDto;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AccountDto2EntityConverter implements ComparativePatchConverter<AccountDto, AccountEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 5;

    private List<String> fieldsToEscape;

    @Value("#{'${res.customer.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private String dobFormat;

    @Value("${res.customer.dob.format}")
    public void setDobFormat(String dobFormat) {
        this.dobFormat = dobFormat;
    }

    @Override
    public void compareAndMap(AccountDto dto, AccountEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optGenderId = dto.getGenderId();
        if(!fieldsToEscape.contains("genderId") && optGenderId.isPresent()) {
            Long genderId = Long.parseLong(optGenderId.get());
            actualEntity.setGenderId(genderId);
            changeSW[i++] = true;
            log.debug("AccountDto.genderId is valid");
        }
        Optional<String> optFirstName = dto.getFirstName();
        if(!fieldsToEscape.contains("firstName") && optFirstName.isPresent()) {
            actualEntity.setFirstName(optFirstName.get());
            changeSW[i++] = true;
            log.debug("AccountDto.firstName is valid");
        }
        Optional<String> optLastName = dto.getLastName();
        if(!fieldsToEscape.contains("lastName") && optLastName.isPresent()) {
            actualEntity.setLastName(optLastName.get());
            changeSW[i++] = true;
            log.debug("AccountDto.lastName is valid");
        }
        Optional<String> optDateOfBirth = dto.getDateOfBirth();
        if(!fieldsToEscape.contains("dateOfBirth") && optDateOfBirth.isPresent()) {
            DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dobFormat);
            LocalDate dateOfBirth = LocalDate.parse(optDateOfBirth.get(), dtf);
            actualEntity.setDateOfBirth(dateOfBirth);
            changeSW[i++] = true;
            log.debug("AccountDto.dateOfBirth is valid");
        }
        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("AccountDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided AccountDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided AccountDto attributes are valid");
    }

}
