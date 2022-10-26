package com.teenthofabud.restaurant.solution.engagement.checkin.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
public class CheckInHistoryDocument extends TOABBaseDocument implements Comparable<CheckInHistoryDocument> {

    private CheckInStatus status;

    @Override
    public int compareTo(CheckInHistoryDocument o) {
        return this.getStatus().compareTo(o.getStatus());
    }
}
