package com.teenthofabud.restaurant.solution.establishmentarea.table.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableException;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableForm;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.TableVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface TableService {

    public String createTable(TableForm form) throws TableException;

    public List<TableVo> retrieveListOfAllTables() throws TableException;

    public TableVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TableException;

    public List<TableVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTableName,
                                                              Optional<String> description,
                                                              Optional<String> capacity) throws TableException;

    public void updateTable(String id, TableForm form) throws TableException;

    public void deleteTable(String id) throws TableException;

    public void applyPatchOnTable(String id, List<PatchOperationForm> patches) throws TableException;

    public List<TableVo> retrieveAllMatchingDetailsByFloorId(String floorId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TableException;
}
