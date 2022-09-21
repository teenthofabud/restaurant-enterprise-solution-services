package com.teenthofabud.restaurant.solution.engagement.checkin.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInEntity;

import java.time.LocalDateTime;
import java.util.Optional;

public interface CheckInRepository<T extends CheckInEntity> extends TOABSimpleEntityBaseRepository<T> {

    public Optional<? extends CheckInEntity> findBySequenceAndCreatedOnBetween(Long sequence, LocalDateTime start, LocalDateTime end);

    public Boolean existsByAccountIdAndSequence(String accountId, String sequence);

    public Optional<? extends CheckInEntity> findByCreatedOnBetween(LocalDateTime start, LocalDateTime end);

}
