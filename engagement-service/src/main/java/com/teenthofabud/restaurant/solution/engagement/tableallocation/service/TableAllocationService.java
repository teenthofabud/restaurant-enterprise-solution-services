package com.teenthofabud.restaurant.solution.engagement.tableallocation.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationException;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationForm;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface TableAllocationService {

    public Set<TableAllocationVo> retrieveAllByNaturalOrdering();

    public TableAllocationVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TableAllocationException;

    public List<TableAllocationVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTableId,
                                                                Optional<String> optionalActive,
                                                                Optional<String> optionalNotes) throws TableAllocationException;

    public List<TableAllocationVo> retrieveAllMatchingDetailsByCheckInId(String checkInId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TableAllocationException;

    public String createTableAllocation(TableAllocationForm form) throws TableAllocationException;

    public void updateTableAllocation(String id, TableAllocationForm form) throws TableAllocationException;

    public void deleteTableAllocation(String id) throws TableAllocationException;

    public void applyPatchOnTableAllocation(String id, List<PatchOperationForm> patches) throws TableAllocationException;

}
