package com.bcdm.foodtraceability.service.impl;

import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.entity.UserInfo;
import com.bcdm.foodtraceability.mapper.UserMapper;
import com.bcdm.foodtraceability.service.UserService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-11
 */
@Service
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

    @Override
    public UserInfo login(User user) {
        return null;
    }

    @Override
    public UserInfo register(User user, UserInfo userInfo) {
        if(saveOrUpdate(user)){

        }
        return null;
    }

    @Override
    public UserInfo modifyPassword(User user) {
        return null;
    }

    @Override
    public UserInfo modifyUserInfo(UserInfo userInfo) {
        return null;
    }
}
