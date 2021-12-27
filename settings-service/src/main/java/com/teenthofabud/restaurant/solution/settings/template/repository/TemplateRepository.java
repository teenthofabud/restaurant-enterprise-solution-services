package com.teenthofabud.restaurant.solution.settings.template.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.settings.template.data.TemplateDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TemplateRepository extends MongoRepository<TemplateDocument, String>, TOABBaseMongoRepository {

    public TemplateDocument save(TemplateDocument entity);

    Boolean existsByNameAndTemplateTypeId(String name, String templateTypeId);

}
