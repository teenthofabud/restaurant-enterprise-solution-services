package com.teenthofabud.restaurant.solution.engagement.tableallocation.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.CheckInRepository;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationDto;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TableAllocationDto2EntityConverter implements ComparativePatchConverter<TableAllocationDto, TableAllocationEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;

    private List<String> fieldsToEscape;

    private CheckInRepository checkInRepository;

    @Autowired
    public void setCheckInRepository(CheckInRepository checkInRepository) {
        this.checkInRepository = checkInRepository;
    }

    @Value("#{'${res.engagement.tableAllocation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }


    @Override
    public void compareAndMap(TableAllocationDto dto, TableAllocationEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        /*Optional<String> optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("tableId") && optTableId.isPresent()) {
            actualEntity.setTableId(optTableId.get());
            changeSW[i++] = true;
            log.debug("TableAllocationDto.tableId is valid");
        }

        Optional<String> optStatus = dto.getStatus();
        if(!fieldsToEscape.contains("status") && optStatus.isPresent()) {
            TableAllocationStatus status = TableAllocationStatus.valueOf(optStatus.get());
            actualEntity.addStatus(status);
            changeSW[i++] = true;
            log.debug("TableAllocationDto.status is valid");
        }*/

        Optional<String> optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("tableId") && optTableId.isPresent()) {
            actualEntity.setTableId(optTableId.get());
            changeSW[i++] = true;
            log.debug("TableAllocationDto.tableId is valid");
        }

        Optional<String> optNotes = dto.getNotes();
        if(!fieldsToEscape.contains("notes") && optNotes.isPresent()) {
            actualEntity.setNotes(optNotes.get());
            changeSW[i++] = true;
            log.debug("TableAllocationDto.notes is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("TableAllocationDto.active is valid");
        }

        Optional<String> optCheckInId = dto.getCheckInId();
        if(!fieldsToEscape.contains("checkInId") && optCheckInId.isPresent()) {
            Long checkInId = Long.parseLong(optCheckInId.get());
            Optional<CheckInEntity> checkInEntityOptional = checkInRepository.findById(checkInId);
            actualEntity.setCheckIn(checkInEntityOptional.get());
            changeSW[i++] = true;
            log.debug("TableAllocationDto.checkInId is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TableAllocationDto attributes are valid");
            this.compareAndMap(dto, actualEntity);
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TableAllocationDto attributes are valid");
    }

}
