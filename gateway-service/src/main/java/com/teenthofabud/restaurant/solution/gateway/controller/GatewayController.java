package com.teenthofabud.restaurant.solution.gateway.controller;

//import com.teenthofabud.core.common.data.vo.ErrorVo;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GatewayController {

//@GetMapping("/establishmentServiceFallBack")
//public ErrorVo establishmentServiceFallBackMethod(){
//    ErrorVo vo = new ErrorVo();
//    vo.setCode("404");
//    vo.setMessage("establishmentService is taking longer than expected, please try again later!!!");
//    return vo;
//}

@GetMapping("/establishmentServiceFallBack")
public String establishmentServiceFallBackMethod(){
    return "establishmentService is taking longer than expected, please try again later!!!";
}

}
