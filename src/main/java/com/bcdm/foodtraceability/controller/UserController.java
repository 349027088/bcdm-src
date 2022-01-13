package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import org.springframework.web.bind.annotation.*;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/user")
public class UserController {

    @RequestMapping("/login")
    public ReturnItem<User> login(@RequestBody User user) throws Exception{
        if ("1".equals(user.getLoginId())){

            throw  new ServiceBusinessException("400","链接失败");
        }
        return null;
    }


}

