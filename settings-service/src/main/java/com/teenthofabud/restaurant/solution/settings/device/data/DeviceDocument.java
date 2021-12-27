package com.teenthofabud.restaurant.solution.settings.device.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("device")
public class DeviceDocument extends TOABBaseDocument implements Comparable<DeviceDocument> {

    @Id
    @Indexed
    private String id;
    private String name;
    private String description;
    private String location;
    private String deviceTypeId;

    @Override
    public int compareTo(DeviceDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
