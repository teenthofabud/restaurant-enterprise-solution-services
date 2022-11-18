package com.teenthofabud.restaurant.solution.encounter.meeting.repository;

import com.teenthofabud.core.common.repository.TOABSimpleEntityBaseRepository;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingEntity;

import java.time.LocalDateTime;
import java.util.Optional;

public interface MeetingRepository<T extends MeetingEntity> extends TOABSimpleEntityBaseRepository<T> {

    public Optional<T> findBySequenceAndCreatedOnBetween(Long sequence, LocalDateTime start, LocalDateTime end);

    public Boolean existsByAccountIdAndSequence(String accountId, String sequence);
}
