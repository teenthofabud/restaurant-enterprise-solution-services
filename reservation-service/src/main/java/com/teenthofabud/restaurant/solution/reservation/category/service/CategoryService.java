package com.teenthofabud.restaurant.solution.reservation.category.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryForm;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface CategoryService {

    public Set<CategoryVo> retrieveAllByNaturalOrdering();

    public CategoryVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CategoryException;

    public List<CategoryVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName,
                                                                 Optional<String> optionalDescription) throws CategoryException;

    public String createCategory(CategoryForm form) throws CategoryException;

    public void updateCategory(String id, CategoryForm form) throws CategoryException;

    public void deleteCategory(String id) throws CategoryException;

    public void applyPatchOnCategory(String id, List<PatchOperationForm> patches) throws CategoryException;

}
