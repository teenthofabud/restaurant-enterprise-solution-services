package com.teenthofabud.restaurant.solution.establishmentarea.table.converter;

import com.teenthofabud.restaurant.solution.establishmentarea.floor.data.FloorEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.floor.repository.FloorRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableEntity;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class TableForm2EntityConverter implements Converter<TableForm, TableEntity> {

    private List<String> fieldsToEscape;
    private FloorRepository floorRepository;

    @Value("#{'${res.establishment.area.table.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setFloorRepository(FloorRepository floorRepository) {
        this.floorRepository = floorRepository;
    }

    @Override
    public TableEntity convert(TableForm form) {
        TableEntity entity = new TableEntity();
        if(!fieldsToEscape.contains("tableName")) {
            entity.setTableName(form.getTableName());
        }
        if(!fieldsToEscape.contains("description")) {
            entity.setDescription(form.getDescription());
        }
        if(!fieldsToEscape.contains("capacity")) {
            entity.setCapacity(form.getCapacity());
        }
        if(!fieldsToEscape.contains("floorId")) {
            Long floorId = Long.parseLong(form.getFloorId());
            Optional<FloorEntity> optFloorEntity = floorRepository.findById(floorId);
            entity.setFloor(optFloorEntity.get());
        }
        entity.setActive(Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }

}
