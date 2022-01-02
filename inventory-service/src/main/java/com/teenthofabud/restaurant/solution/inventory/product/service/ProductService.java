package com.teenthofabud.restaurant.solution.inventory.product.service;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductException;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductForm;
import com.teenthofabud.restaurant.solution.inventory.product.data.ProductVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public interface ProductService {

    public Set<ProductVo> retrieveAllByNaturalOrdering();

    public ProductVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ProductException;

    public List<ProductVo> retrieveAllMatchingDetailsByCategoryId(String categoryId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ProductException;

    public List<ProductVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalDescription) throws ProductException;

    public String createProduct(ProductForm form) throws ProductException;

    public void updateProduct(String id, ProductForm form) throws ProductException;

    public void deleteProduct(String id) throws ProductException;

    public void applyPatchOnProduct(String id, List<PatchOperationForm> patches) throws ProductException;

}
