package com.teenthofabud.restaurant.solution.booking.association.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AssociationRepository extends MongoRepository<AssociationDocument, String>, TOABBaseMongoRepository {

    public List<AssociationDocument> findByExperienceId(String experienceId);

    public Boolean existsByTableIdAndAccountId(String tableId, String accountId);

}
