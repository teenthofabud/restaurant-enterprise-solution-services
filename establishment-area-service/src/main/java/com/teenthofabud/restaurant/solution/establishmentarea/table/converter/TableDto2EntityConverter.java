package com.teenthofabud.restaurant.solution.establishmentarea.table.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableDto;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.repository.TableRepository;
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
public class TableDto2EntityConverter implements ComparativePatchConverter<TableDto, TableEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 5;

    private List<String> fieldsToEscape;

    @Value("#{'${res.establishment.area.table.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    private FloorRepository floorRepository;

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Override
    public void compareAndMap(TableDto dto, TableEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        Optional<String> optTableName = dto.getTableName();
        if(!fieldsToEscape.contains("tableName") && optTableName.isPresent()) {
            actualEntity.setTableName(optTableName.get());
            changeSW[i++] = true;
            log.debug("TableDto.tableName is valid");
        }

        Optional<String> optDesc = dto.getDescription();
        if(!fieldsToEscape.contains("description") && optDesc.isPresent()) {
            actualEntity.setDescription(optDesc.get());
            changeSW[i++] = true;
            log.debug("TableDto.description is valid");
        }

        Optional<String> optCapacity = dto.getCapacity();
        if(!fieldsToEscape.contains("capacity") && optCapacity.isPresent()) {
            actualEntity.setCapacity(optCapacity.get());
            changeSW[i++] = true;
            log.debug("TableDto.capacity is valid");
        }

        Optional<String> optFloorId = dto.getFloorId();
        if(!fieldsToEscape.contains("floorId") && optFloorId.isPresent()) {
            Long accountId = Long.parseLong(optFloorId.get());
            Optional<FloorEntity> optAccountEntity = floorRepository.findById(accountId);
            actualEntity.setFloor(optAccountEntity.get());
            changeSW[i++] = true;
            log.debug("TableDto.floorId is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("TableDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided TableDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided TableDto attributes are valid");
    }

}
