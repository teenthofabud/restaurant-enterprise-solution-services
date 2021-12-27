package com.teenthofabud.restaurant.solution.settings.device.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;


@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DeviceForm {

    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private String deviceTypeId;
    @ToString.Include
    private String location;

}
