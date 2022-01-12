package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.segments.MergeSegments;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.mapper.UserMapper;
import com.bcdm.foodtraceability.service.UserService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.val;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.CreateMD5.Md5encode;
import static com.bcdm.foodtraceability.common.CreateUUID.getUUID;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {

    @Override
    public User login(User user) {
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("loginId", user.getLoginId());
        User selectUser = getOne(queryWrapper);
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(user.getPassword());
        stringBuilder.append(selectUser.getSalt());
        if (selectUser.getPassword().equals(Md5encode(stringBuilder.toString()))) {
            return user;
        }
        return null;


    }

    @Override
    public User register(User user) {
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("loginId", user.getLoginId());
        if (0 == count(queryWrapper)){
            user.setSalt(getUUID());
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append(user.getPassword());
            stringBuilder.append(user.getSalt());
            user.setPassword(Md5encode(stringBuilder.toString()));
            LocalDateTime now = LocalDateTime.now();
            user.setCreateTime(now);
            user.setUpdateTime(now);
            save(user);
           return login(user);
        }
        return null;
    }

    @Override
    public User modifyPassword(User user,String newPassword) {
        User targetUser = login(user);
        user = targetUser;
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(newPassword);
        stringBuilder.append(targetUser.getSalt());
        targetUser.setPassword(Md5encode(stringBuilder.toString()));
        UpdateWrapper updateWrapper = new UpdateWrapper();
        updateWrapper.eq("userId",targetUser.getUserId());
        updateWrapper.eq("updateTime",targetUser.getUpdateTime());
        return update(targetUser,updateWrapper)?targetUser:user;
    }

    @Override
    public User modifyUserInfo(User user) {
        User targetUser = login(user);
        UpdateWrapper updateWrapper = new UpdateWrapper();
        updateWrapper.eq("userId",targetUser.getUserId());
        updateWrapper.eq("updateTime",targetUser.getUpdateTime());
        return update(user,updateWrapper)?user:targetUser;
    }
}
