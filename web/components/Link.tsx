import NextLink from 'next/link';
import { FC } from 'react';

type Props = {
  href: string;
  className?: string;
};

const Link: FC<Props> = ({ href, className, children }) => {
  return (
    <NextLink href={href} passHref={true}>
      <a className={className}>{children}</a>
    </NextLink>
  );
};

export default Link;
