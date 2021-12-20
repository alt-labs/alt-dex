import NextImage, { ImageProps } from 'next/image';
import { FC } from 'react';
import styled from 'styled-components';

type Props = ImageProps & {
  className?: string;
};

const ImageWrapper = styled.div`
  background: transparent;
`;

const Image: FC<Props> = ({ className, ...props }) => {
  return (
    <ImageWrapper className={className}>
      <NextImage className={className} {...props} />
    </ImageWrapper>
  );
};

export default Image;
